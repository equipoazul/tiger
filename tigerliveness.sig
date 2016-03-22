signature tigerliveness =
sig

    (*type liveSet = (tigergraph.node, unit) tigertab.Tabla * tigertemp.temp list*)
    (*type liveSet = tigergraph.node Splayset.set * tigertemp.temp list
    type liveMap = (tigergraph.node, liveSet) tigertab.Tabla*)
    type liveSet = (tigergraph.node, string Splayset.set) tigertab.Tabla

    datatype igraph =
        IGRAPH of {graph: tigergraph.graph,
                   tnode: tigertemp.temp -> tigergraph.node,
                   gtemp: tigergraph.node -> tigertemp.temp,
                   moves: (tigergraph.node * tigergraph.node) list}
                   
    (*val interferenceGraph : tigerflow.flowgraph -> igraph * (tigergraph.node -> tigertemp.temp list)*)
    val liveAnalysis: tigerflow.flowgraph * tigergraph.node list -> unit 
    
    val liveIn : liveSet ref
    val liveOut : liveSet ref

end

