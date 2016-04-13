structure tigercoloring :> tigercoloring =
struct
    open Array
    open tigerutils
    open tigerliveness
    open tigerflow
    open tigergraph
    open tigertab
    open tigerassem
    open tigerframe
    
    type adjListT    = (tigergraph.node Splayset.set) array
    type wListMoves  = tigergraph.node Splayset.set ref
    type adjSetT     = tigergraph.edge Splayset.set ref
    
    val precolored = listToSet String.compare [fp, sp, rv, ov, "ebx", "ecx"]
    val initial = ref (Splayset.empty String.compare)
    
    fun coloring (b, f, firstRun) =
      let
		val (FGRAPH fg, iTable) = instrs2graph b
        val (liveIn, liveOut) = liveAnalysis (FGRAPH fg) 
        val uses = List.concat (tabValores (!(#use fg)))
        val defs = List.concat (tabValores (!(#def fg)))
        val totalRegs = unionList uses defs

        val (IGRAPH ig) = newInterGraph()
        val _ = insertNodesLiv (IGRAPH ig) totalRegs
        val interNodes = nodes (#graph ig)
        
        val flowNodes = nodes (#control fg)
        val lenNodes = List.length(interNodes)
        val moveList = array(lenNodes, Splayset.empty tupleCompare)
        val alias = array(lenNodes, ~1)
        val color = array(lenNodes, "noColor")
        val spillWorklist = ref (Splayset.empty Int.compare)
        val freezeWorklist = ref (Splayset.empty Int.compare)
        val simplifyWorklist = ref (Splayset.empty Int.compare)

        val worklistMoves = ref (Splayset.empty tupleCompare)
        val activeMoves = ref (Splayset.empty tupleCompare)
        val coalescedMoves = ref (Splayset.empty tupleCompare)
        val frozenMoves = ref (Splayset.empty tupleCompare)
        val constrainedMoves = ref (Splayset.empty tupleCompare)

        val coalescedNodes = ref (Splayset.empty Int.compare)
        val coloredNodes = ref (Splayset.empty Int.compare)
        val spilledNodes = ref (Splayset.empty Int.compare)



        val selectStack: node stack ref = ref tigerutils.emptyStack 
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
                                      NONE => (Splayset.empty String.compare) (*raise Fail ("El nodo " ^ Int.toString(n) ^ " no existe en la tabla use")*)
                                    | SOME u => listToSet String.compare u
                        val def = case tabBusca(n, (!(#def fg))) of
                                      NONE => (Splayset.empty String.compare) (*raise Fail ("El nodo " ^ Int.toString(n) ^ " no existe en la tabla def")*)
                                    | SOME d => listToSet String.compare d
                        val _ = case i of
                                     MOVE {assem=a, src=u, dst=v} => 
                                               let
                                                 val _ = live := Splayset.difference (!live, use)
                                                 val union = Splayset.listItems (Splayset.union(use, def))
                                                 val _ = List.map (fn x => case tabBusca(x, !(#tnode ig)) of
                                                                               NONE => raise Fail ("El temp " ^ x ^ " no esta en la tabla tnode")
                                                                             | SOME xi => update(moveList, xi,  Splayset.add(sub(moveList, xi), (u, v) ))) union

                                                 val _ = Splayset.add(!worklistMoves, (u,v))
                                               in
                                                 ()
                                               end 
                                    | _ => ()

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
             (*tigerflow.printGraphFlow(FGRAPH fg);
             List.map updateLive (List.rev (ListPair.zip (b, flowNodes)))*)
             List.map updateLive (List.rev (ListPair.zip (b, flowNodes)))
             end

        fun nodeMoves n = Splayset.intersection(sub(moveList, n), (Splayset.union(!activeMoves, !worklistMoves)))
        fun moveRelated n = not (Splayset.isEmpty (nodeMoves n))
        
        (* 6 es el K, y hay que pasarle una lista de todos los temporales (el initial) *)
        fun   makeWorklist () = 
          let
            fun makeWorklist' m =
                let 
                   val n = tempToNode (IGRAPH ig) m
                in
                   if sub(degrees, n) >= k then
                      spillWorklist := Splayset.add(!spillWorklist, n)
                   else if moveRelated(n) then
                      freezeWorklist := Splayset.add(!freezeWorklist, n)
                   else
                      simplifyWorklist := Splayset.add(!simplifyWorklist, n)
                end
          in
             Splayset.app makeWorklist' (!initial)
          end



        fun adjacent n = Splayset.difference(sub(adjList, n), Splayset.union(stackToSet Int.compare (!(selectStack)), !coalescedNodes))

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
          
        fun simplify () =
            let
              val n = List.hd (Splayset.listItems (!simplifyWorklist))
            in
             (simplifyWorklist := Splayset.delete(!simplifyWorklist, n);
              tigerutils.push n (selectStack);
              Splayset.app (fn x => decrementDegree x) (adjacent n))
            end

        (* Coalesced *)

        fun getAlias n =
            if Splayset.member(!coalescedNodes, n) then
               getAlias (sub(alias, n))
            else
               n

        fun combine u v =
           (if Splayset.member(!freezeWorklist, v) then
              freezeWorklist := Splayset.delete(!freezeWorklist, v)
            else
              spillWorklist := Splayset.delete(!spillWorklist, v);
            coalescedNodes := Splayset.add(!coalescedNodes, v);
            update(alias, v, u);
            update(moveList, u, Splayset.union(sub(moveList, u), sub(moveList, v)));
            enableMoves(Splayset.singleton Int.compare v);
            Splayset.app (fn t => (addEdge({from=t, to=u}); decrementDegree t)) (adjacent v);
            if sub(degrees, u) >= k andalso Splayset.member(!freezeWorklist, u) then
               (freezeWorklist := Splayset.delete(!freezeWorklist, u);
                spillWorklist := Splayset.add(!spillWorklist, u))
            else
                ())
            
        fun addWorkList u =
           let 
               val unode = case tabBusca(u, !(#tnode ig)) of
                               NONE => raise Fail "No se encontro el nodo (addWorkList)"
                             | SOME s => s
           in
               if Splayset.member(precolored, u) andalso not (moveRelated unode) andalso sub(degrees, unode) < k then
                   (freezeWorklist := Splayset.delete(!freezeWorklist, unode);
                    simplifyWorklist := Splayset.add(!simplifyWorklist, unode))
               else
                  ()
           end
           
        fun ok (t, r) =
          let
             val tnod = case tabBusca(t, !(#tnode ig)) of
                           NONE => raise Fail "No se encontro el nodo (ok - t)"
                         | SOME n => n
                         
             val rnod = case tabBusca(t, !(#tnode ig)) of
                           NONE => raise Fail "No se encontro el nodo (ok - r)"
                         | SOME n => n
          in
             sub(degrees, tnod) < k orelse Splayset.member(precolored, t) orelse Splayset.member(!adjSet, {from=tnod, to=rnod})
          end
          
        fun conservative cn =
            let 
                val k' = ref 0
                val _ = Splayset.app (fn n => if sub(degrees, n) > k then k' := !k' + 1 else ()) cn
             in
                !k' < k
             end
             
        (* En todos los lugares donde decia m en el libro puse (x,y) pero podria ser (x', y') revisar *)
        fun coalesce () =
            let
               fun coalesce' (x, y) =
                   let
                      val x' = nodeToTemp (IGRAPH ig) (getAlias(tempToNode (IGRAPH ig) x))
                      val y' = nodeToTemp (IGRAPH ig) (getAlias(tempToNode (IGRAPH ig) y))

                      val (u, v) = if Splayset.member(precolored, y) then
                                      (y', x')
                                   else
                                      (x', y')

                      val m = (x', y')

                      val (uNode, vNode) = (tempToNode (IGRAPH ig) u, tempToNode (IGRAPH ig) u)

                      val adjacentTmp = Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (adjacent vNode)
                      val checkForall = Splayset.foldr (fn (t, xs) => (ok (t, u)) andalso xs) true adjacentTmp
                   in
                     (worklistMoves := Splayset.delete(!worklistMoves, m);
                      if u = v then
                        (coalescedMoves := Splayset.add(!coalescedMoves, m);
                         (addWorkList u))
                      else if Splayset.member(precolored, v) orelse Splayset.member(!adjSet, {from=uNode, to=vNode}) then
                         (constrainedMoves := Splayset.add(!constrainedMoves, (u, v));
                          (addWorkList u);
                          (addWorkList v))
                      else if (Splayset.member(precolored, u) andalso checkForall) orelse 
                              (not(Splayset.member(precolored, u)) andalso conservative(Splayset.union(adjacent uNode, adjacent vNode))) then
                               (coalescedMoves := Splayset.add(!coalescedMoves, m);
                                (combine uNode vNode);
                                (addWorkList u))
                      else
                        activeMoves := Splayset.add(!activeMoves, m))                                         
                   end 
            in
                Splayset.app coalesce' (!worklistMoves)
            end

        fun freezeMoves u =
          let
            fun processNodeMove m =
              let
                val (x, y) = m
                val yAlias = case tabBusca(y, !(#tnode ig)) of
                                  NONE => raise Fail "No se encontro el nodo (freezeMoves)"
                                | SOME s => (getAlias s)
                val xAlias = case tabBusca(x, !(#tnode ig)) of
                                  NONE => raise Fail "No se encontro el nodo (freezeMoves)"
                                | SOME s => (getAlias s)
                val uAlias = getAlias u
                val v = if yAlias = uAlias then xAlias
                        else yAlias
              in
                (activeMoves := Splayset.delete(!activeMoves, m);
                 frozenMoves := Splayset.add(!frozenMoves, m);
                 if (Splayset.isEmpty (nodeMoves v)) andalso sub(degrees, v) < k then
                   (freezeWorklist := Splayset.delete(!freezeWorklist, v);
                    simplifyWorklist := Splayset.add(!simplifyWorklist, v))
                 else
                   ())
              end
          in
            Splayset.app processNodeMove (nodeMoves u)
          end

        fun freeze () =
          let
            fun freeze' u = (freezeWorklist := Splayset.delete(!freezeWorklist, u);
                             simplifyWorklist := Splayset.add(!simplifyWorklist, u);
                             freezeMoves u)
                    
          in
            Splayset.app freeze' (!freezeWorklist)
          end 

        fun selectSpill () =
          let 
       	  	fun spillPriority (n) =
       	  		let
       	  			val nDef = List.foldr (fn ((x, y), xs) => if tigerutils.inList (nodeToTemp (IGRAPH ig) n)  y then 1.0 + xs else xs) 0.0 (tigertab.tabAList (!(#def fg)))
       	  			val nUse = List.foldr (fn ((x, y), xs) => if tigerutils.inList (nodeToTemp (IGRAPH ig) n) y then 1.0 + xs else xs) 0.0 (tigertab.tabAList (!(#use fg)))
       	  			val nAdj = Splayset.numItems(sub(adjList, n))
       	  		in
       	  			(nDef + nUse) / real(nAdj)
       	  		end
   	  		
   	  		fun minElem [] min node = node
   	  		  | minElem ((x, y)::xs) min node =
  					if x < min then minElem xs x y 
	   	  			else minElem xs min node
       	  	
       	  	val priorities = List.map (fn x => (spillPriority(x), x)) (Splayset.listItems (!spillWorklist))
            val m = minElem priorities 9999.0 ~1
          in
            (spillWorklist := Splayset.delete(!spillWorklist, m);
             simplifyWorklist := Splayset.add(!simplifyWorklist, m);
             freezeMoves m)
          end


        fun assignColors () =
          let 
            (*val okColors = ref ([0,1,2,3,4,5]) (* Si, habria que hacerlo con compresiones de listas *)*)
            val okColors = ref (["eax","ebx","ecx","edx","edi","esi"])
            val n = ref ~1 (* Valor dummy para usar en el pop *)
            fun processAdjList n = Splayset.app (fn w => let
                                                            val wAlias = getAlias w
                                                            val wtmp = nodeToTemp (IGRAPH ig) wAlias 
                                                            val coloredNodesTmp = Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (!coloredNodes)
                                                          in
                                                            if Splayset.member(Splayset.union(coloredNodesTmp, precolored), wtmp) then
                                                                okColors := List.filter (fn x => x <> sub(color, wAlias)) (!okColors)
                                                            else 
                                                               ()
                                                          end) (sub(adjList, n))
          
          
          in
            (while (not(isEmptyStack selectStack)) do
                (n := pop selectStack;
                 processAdjList (!n);
                 if List.null(!okColors) then
                    (spilledNodes := Splayset.add(!spilledNodes, (!n));
                     print ("Spillie " ^ Int.toString(!n) ^ "\n"))
                 else
                    (coloredNodes := Splayset.add(!coloredNodes, (!n));
                     update(color, (!n), List.hd (!okColors));
                     okColors := List.tl (!okColors) ));
             Splayset.app (fn m => update(color, m, sub(color, getAlias m))) (!coalescedNodes))
                 
          end      

		        
		fun rewriteProgram() =   
			let 
				val _ = print ("Initial antes rewrite\n\n")
				val _ = Splayset.app (fn x => print (x ^ "\n")) (!initial)
				val newTemps = ref (Splayset.empty String.compare)
				fun rewriteProgram' (temp, blocks) =
					let 
						fun getUseDefs l = List.foldr (fn ((x, y), xs) => let
																			val instr = case tabBusca(x, iTable) of
																							      NONE => raise Fail "No se encontro el nodo en la iTable"
																							    | SOME i => case i of
 																							    			  MOVE {assem=ass,dst=d,src=s} => {dst=d, src=s}
																							    		    | _ => raise Fail "No es un move (getUseDefs)"
																		  in
	  																	    if inList temp y then ((x, instr)::xs) else xs
																		  end) [] l
																		  
						val defs = getUseDefs (tigertab.tabAList(!(#def fg)))
						(*val uses = getUseDefs (tigertab.tabAList(!(#use fg)))*)
						 
						fun rewriteDef (nodo, instr) bls =
							let 
								val m = case (allocLocal f true) of
									InFrame m' => Int.toString(m')
									| _ => raise Fail "En true esto no deberia pasar...."

								val t = tigertemp.newtemp()
								val _ = newTemps := Splayset.add(!newTemps, t)
								val store = [MOVE {assem="movl "^t^","^(#src instr), dst=t, src=(#src instr)},
											 MOVE {assem="movl (%ebp-"^m^"),"^t, dst=m, src=t}]
							in
							 	(List.take(bls, nodo - 1) @ store @ List.drop(bls, nodo), m)
							end
							
						fun rewriteUse ((nodo, instr), bls, m) =
							let 
								val t = tigertemp.newtemp()
								val _ = newTemps := Splayset.add(!newTemps, t)
								val fetch = [MOVE {assem="movl "^t^",("^m^")", dst=m, src=t},
											 MOVE {assem="movl "^(#dst instr)^", (%ebp-"^m^")", dst=(#dst instr), src=m}]
							in
							 	List.take(bls, nodo - 1) @ fetch @ List.drop(bls, nodo)
							end
							
						fun rewriteAll (nodo, instr) bls =
							let 
								val (bs, m) = rewriteDef (nodo, instr) blocks
							in
								rewriteUse ((nodo, instr), bs, m)
							end
						
					in
						List.foldr (fn (x, xs) => rewriteAll x xs) blocks defs		
					end	 
				val _ = "Leus puto\n"
				val rewritedProgram = Splayset.foldr (fn (x, xs) => rewriteProgram' (nodeToTemp (IGRAPH ig) x, xs)) b (!spilledNodes)

				(* Pasamos los nodos de numero a temps *)
				val coloredNodesTmp = ref (Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (!coloredNodes))
				val coalescedNodesTmp = ref (Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (!coalescedNodes))
			in
				(initial := Splayset.union(!coloredNodesTmp, Splayset.union(!coalescedNodesTmp, !newTemps));
				 print ("DESPUES antes rewrite\n\n");
				 Splayset.app (fn x => print (x ^ "\n")) (!initial);
				 rewritedProgram)
			end
			
		fun printColorArray a 0 = print (Int.toString(0) ^ " (" ^ (nodeToTemp (IGRAPH ig) 0) ^ ")" ^ " -> " ^ Array.sub(a, 0) ^ "\n")
		  | printColorArray a n = (print (Int.toString(n) ^ " (" ^ (nodeToTemp (IGRAPH ig) n) ^ ")" ^ " -> " ^ Array.sub(a, n) ^ "\n"); printColorArray a (n - 1))
		
      in
        (build ();
         if firstRun then
	         initial := List.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (nodes (#graph ig))
	     else
	     	();
         makeWorklist();
         while not(Splayset.isEmpty(!simplifyWorklist) andalso Splayset.isEmpty(!worklistMoves) andalso
                   Splayset.isEmpty(!freezeWorklist) andalso Splayset.isEmpty(!spillWorklist)) do
               if not(Splayset.isEmpty(!simplifyWorklist)) then simplify()
               else if not(Splayset.isEmpty(!worklistMoves)) then coalesce()
               else if not(Splayset.isEmpty(!freezeWorklist )) then freeze()
               else if not(Splayset.isEmpty(!spillWorklist )) then selectSpill() 
               else ();
         assignColors();
         printColorArray color (lenNodes - 1);
         if not(Splayset.isEmpty(!spilledNodes)) then
         	(print "Me llamo de nuevo pue\n";
         	coloring(rewriteProgram(), f, false))
         else
         	(b, f))

         

      end



end
