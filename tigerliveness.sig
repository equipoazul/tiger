signature tigerliveness =
sig

    (*type liveSet = (tigergraph.node, unit) tigertab.Tabla * tigertemp.temp list*)
    (*type liveSet = tigergraph.node Splayset.set * tigertemp.temp list
    type liveMap = (tigergraph.node, liveSet) tigertab.Tabla*)
    type liveSet = (tigergraph.node, string Splayset.set) tigertab.Tabla

    datatype igraph =
        IGRAPH of {graph: tigergraph.graph,
                   tnode: (tigertemp.temp,tigergraph.node) tigertab.Tabla ref,
                   gtemp: (tigergraph.node, tigertemp.temp) tigertab.Tabla ref,
                   moves: (tigergraph.node * tigergraph.node) list ref}
                   
    val newInterGraph: unit -> igraph
    val liveAnalysis: tigerflow.flowgraph -> liveSet * liveSet 

    val nodeToTemp: igraph -> int -> string
    val tempToNode: igraph -> string -> int
    
    val insertNodesLiv: igraph -> tigertemp.temp list -> unit

end

