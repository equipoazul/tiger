structure tigercoloring :> tigercoloring =
struct
	
	type listMoves : (tigergraph.node, tigerassem.instr list) tigertab.Tabla
	val moveList = ref (tabNueva())
	fun initList ns l = List.map (fn x => l := tabInserta(x, [], !l)) ns
	
	fun build (b::bs) (FGRAPH fg) c =	
		let 
			fun listToSet l = 
			  let
				val emptySet = Splayset.empty String.compare
			  in
				Splayset.addList (emptySet, l)
			  end            
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
