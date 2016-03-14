signature tigergraph =
sig
    type node = int
    type edge
    type graph
        
    val nodes: graph -> node list
    val edges: graph -> edge list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list
    val eq: node*node -> bool
    
    val newGraph: unit -> graph
    val newNode: graph -> node
    exception GraphEdge
    val mk_edge : { from: node, to: node } -> unit
    val rm_edge : { from: node, to: node } -> unit
    
    (* Funciones de impresion *)
    val printNodes : node list -> unit
    val printEdges : edge list -> unit
    val printGraph : graph -> unit
    
    val nodename: node -> string (* for debugging *)
    
    val gr: graph
end

