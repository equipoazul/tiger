signature tigercoloring =
sig
    type adjListT    = (tigergraph.node Splayset.set) array
	type wListMoves  = tigergraph.node Splayset.set ref
	type adjSetT     = tigergraph.edge Splayset.set ref

	
    val color : tigerflow.flowgraph -> (tigerassem.instr list) list -> unit
   
end
