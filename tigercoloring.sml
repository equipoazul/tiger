structure tigercoloring :> tigercoloring =
struct
    open tigerutils
    open tigerliveness
    
	type listMoves   : (tigergraph.node, tigerassem.instr list) tigertab.Tabla
	(* HAcer todo esto como arreglo wacho *)
    type adjListT    : (tigertemp.temp, Splayset.empty String.compare) tigertab.Tabla 
    type degreeT     : (tigertemp.temp, Splayset.empty Int.compare) tigertab.Tabla
	type wListMoves  : tigergraph.node Splayset.set
	
	
	
	fun color () =
	  let
	    val (liveIn, liveOut) = liveAnalysis()
	    val moveList:listMoves ref = ref (tabNueva())
	    val workListMove = Splayset.empty tigerassem.instrCompare
	    val (IGRAPH ig) = newInterGraph()
	    val adjSet = listToSet (edges (#graph ig))
	    val adjList:adjListT = ref (tabNueva())
	    val degrees:degreeT = ref (tabNueva())
	    val preColored = listToSet string.compare ["eax","ebx"]
	                        
	    
	    (* Inicializamos las tablas como vacias *)
	    val _ = List.map (fn x => t := tabInserta(x, Splayset.empty String.compare, !adjList)) ns
	    
	    
        fun addEdge (e as {from=u, to=v}) =
            if Splayset.member (adjSet, e) andalso (u <> v) then
                (adjSet := Splayset.add adjSet {from=u, to=v};
                 adjSet := Splayset.add adjSet {from=v, to=u};
                 if not Splayset.member (precolored, u) then
                    adjList[u] := Splayset.add adjList[u] v;
                    degree[u] := degree[u] + 1;
                 if not Splayset.member (precolored, v) then
                    adjList[v] := Splayset.add adjList[v] u;
                    degree[v] := degree[v] + 1;)
                

	  in
	    
	  end
	
	val moveList
	fun initList ns l = List.map (fn x => l := tabInserta(x, [], !l)) ns

    val adjSet = (#edges (#graph tigerliveness.interGraph))
    val adjList = ref (tabNueva()) : (tigergraph.node, int Splayset.set int.compare) tigertab.Table ref






	
	fun build (b::bs) (FGRAPH fg) c =	
		val moveList = ref (tabNueva())
		let 
			val l = List.length(b)
			val live = List.foldr (fn (s, ss) => let 
			                                      val nodes = List.tabulate(l-c, fn x => x+c);
												  val set = case tabBusca(s, !tigerliveness.liveOut) of
															NONE => raise Fail "La instruccion no esta en la tabla liveOut"
															SOME set => set
    										 in
											  Splayset.union set ss
											 end) {} nodes
			val i = c
			while i < l + c do
				case i of
					MOVE _ => let
								use := case tabBusca(i, #use fg) of
											NONE => raise Fail "El nodo no existe en la tabla use"
											SOME u => listToSet u
								def := case tabBusca(i, #def fg) of
											NONE => raise Fail "El nodo no existe en la tabla def"
											SOME d => listToSet d
								live := Splayset.difference (live, use)
								union := Splayset.toList Splayset.union(use, def)
								
								
	val _ = initList moveList; build .. 	
end
