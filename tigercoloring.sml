structure tigercoloring :> tigercoloring =
struct
    open Array
    open tigerutils
    open tigerliveness
    open tigerflow
    open tigergraph
    open tigertab
    open tigerassem
    
	type listMoves   = (tigergraph.node, tigerassem.instr list) tigertab.Tabla
    type adjListT    = (tigergraph.node Splayset.set) array
	type wListMoves  = tigergraph.node Splayset.set ref
	type instrSet    = tigerassem.instr Splayset.set
	type adjSetT     = tigergraph.edge Splayset.set ref
	
    val precolored = listToSet String.compare ["eax","ebx"]
	
    fun  color (FGRAPH fg) [] = ()
	   | color (FGRAPH fg) (b::bs) =
	  let
		val (liveIn, liveOut) = liveAnalysis (FGRAPH fg) 
		val (IGRAPH ig) = newInterGraph()
		val interNodes = nodes (#graph ig)
		val flowNodes = nodes (#control fg)
		val lenNodes = List.length(interNodes)
		val moveList:instrSet array = array(lenNodes, Splayset.empty tigerassem.instrCompare)
		val spillWorklist = ref (Splayset.empty Int.compare)
		val freezeWorklist = ref (Splayset.empty Int.compare)
		val simplifyWorklist = ref (Splayset.empty Int.compare)
		val worklistMoves = ref (Splayset.empty tigerassem.instrCompare)
		val activeMoves = ref (Splayset.empty tigerassem.instrCompare)
		val coalescedNodes = ref (Splayset.empty Int.compare)
		val selectStack = ref (Splayset.empty Int.compare)
		(*val adjSet:adjSetT = ref (Splayset.empty edgeCompare)*)
		val adjSet:adjSetT = ref (listToSet edgeCompare (edges (#graph ig)))
		val adjList:adjListT = array(lenNodes, Splayset.empty Int.compare)
		val degrees:int array = array(lenNodes, 0)
		val k = 6
					
        fun addEdge (e as {from=u, to=v}) =
            if Splayset.member (!adjSet, e) andalso (u <> v) then
                (adjSet := Splayset.add ((!adjSet), {from=u, to=v});
                 adjSet := Splayset.add ((!adjSet), {from=v, to=u});
                 (case tabBusca(u, !(#gtemp ig)) of
                        NONE => raise Fail "Error al buscar un nodo en la tabla gtemp"
                      | SOME ui => if not (Splayset.member (precolored, ui)) then
                                     (update(adjList, u, Splayset.add(sub(adjList, u), v));
                                      update(degrees, u, sub(degrees, u) + 1))
                                  else
                                     ());
                 (case tabBusca(v, !(#gtemp ig)) of
                        NONE => raise Fail "Error al buscar un nodo en la tabla gtemp"
                      | SOME vi => if not (Splayset.member (precolored, vi)) then
                                      (update(adjList, v, Splayset.add(sub(adjList, v), u));
                                      update(degrees, v, sub(degrees, v) + 1))
                                   else
                                      ()))
             else
                ()
             
        fun build () =	
			let 
				val l = List.length(b)
				val live = ref (List.foldr (fn (s, ss) => (case tabBusca(s, liveOut) of
										NONE => raise Fail "La instruccion no esta en la tabla liveOut"
									  | SOME s' => Splayset.union (s', ss)) ) (Splayset.empty String.compare) interNodes)
												 
				fun updateLive ((i:tigerassem.instr), n) =
					let
                        val use = case tabBusca(n, (!(#use fg))) of
						  NONE => raise Fail "El nodo no existe en la tabla use"
						| SOME u => listToSet String.compare u
                        val def = case tabBusca(n, (!(#def fg))) of
						  NONE => raise Fail "El nodo no existe en la tabla def"
						| SOME d => listToSet String.compare d
					val _ = case i of
							MOVE _ => live := Splayset.difference (!live, use)
							| _ => ()
                        val union = Splayset.listItems (Splayset.union(use, def))
				val _ = List.map (fn x => case tabBusca(x, !(#tnode ig)) of
								NONE => raise Fail "El temp no esta en la tabla tnode"
							  | SOME xi => update(moveList, xi, Splayset.add(sub(moveList, xi), i))) union
				val _ = Splayset.add(!worklistMoves, i)
				val _ = List.map (fn d => case tabBusca(d, !(#tnode ig)) of
							NONE => raise Fail "El temp no esta en la tabla tnode (2)"
							  | SOME di => List.map (fn l => case tabBusca(l, !(#tnode ig)) of
											NONE => raise Fail "El temp no esta en la tabla tnode (3)"
										  | SOME li => addEdge({from=li, to=di})) (Splayset.listItems (!live))) (Splayset.listItems def) 
				val _ = Splayset.union(use , Splayset.difference(!live, def))
                    in
                        ()
                    end
		  
			 in 
			 List.map updateLive (List.rev (ListPair.zip (b, flowNodes)))
			 end

		fun nodeMoves n = Splayset.intersection(sub(moveList, n), (Splayset.union(!activeMoves, !worklistMoves)))
		fun moveRelated n = not (Splayset.isEmpty (nodeMoves n))
		
		(* 6 es el K, y hay que pasarle una lista de todos los temporales (el initial) *)
		fun   makeWorklist [] = ()
			| makeWorklist (n::ns) =
				(if sub(degrees, n) >= k then
					spillWorklist := Splayset.add(!spillWorklist, n)
				 else if moveRelated(n) then
					freezeWorklist := Splayset.add(!freezeWorklist, n)
				 else
					simplifyWorklist := Splayset.add(!simplifyWorklist, n);
				 makeWorklist ns)

		fun adjacent n = Splayset.difference(sub(adjList, n), Splayset.union(!selectStack, !coalescedNodes))

		fun enableMoves nodes = 
			let
				fun f x = (List.map (fn m => if Splayset.member(!activeMoves, m) then
												(activeMoves := Splayset.delete(!activeMoves, m);
												 worklistMoves := Splayset.add(!worklistMoves, m))
										  else
												()) (Splayset.listItems (nodeMoves x)); ())
			in
				Splayset.app f nodes
			end
									 

		fun decrementDegree n =
			let 
				val d = sub(degrees, n)
			in
				(update(degrees, n, d - 1);
				 if d = k then
					(enableMoves (Splayset.add((adjacent n), n));
					 spillWorklist := Splayset.delete(!spillWorklist, n);
					 if moveRelated n then
						freezeWorklist := Splayset.add(!freezeWorklist, n)
					 else
						simplifyWorklist := Splayset.add(!simplifyWorklist, n))
				 else
				 	 ())
			end
		  
		fun simplify n =
			(simplifyWorklist := Splayset.delete(!simplifyWorklist, n);
			 selectStack := Splayset.add(!selectStack, n);
			 Splayset.app (fn x => decrementDegree x) (adjacent n))
	  in
		()
	  end



end
