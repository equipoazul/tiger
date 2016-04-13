signature tigercoloring =
sig
    type adjListT    = (tigergraph.node Splayset.set) array
	type wListMoves  = tigergraph.node Splayset.set ref
	type adjSetT     = tigergraph.edge Splayset.set ref

	
    val coloring : ((tigerassem.instr list) * tigerframe.frame) -> ((tigerassem.instr list) * tigerframe.frame)
   
end
