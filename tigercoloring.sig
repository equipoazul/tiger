signature tigercoloring =
sig
	type listMoves   = (tigergraph.node, tigerassem.instr list) tigertab.Tabla
    type adjListT    = (tigergraph.node Splayset.set) array
	type wListMoves  = tigergraph.node Splayset.set ref
	type adjSetT     = tigergraph.edge Splayset.set ref
	type instrSet    = tigerassem.instr Splayset.set
	
    val color : tigerflow.flowgraph -> (tigerassem.instr list) list -> unit
   
end
