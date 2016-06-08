
signature tigergraph =
sig
    type node = int
    type edge = {from: node, to: node}
    type graph = {nodes: node list ref, edges: edge list ref}
        
    val nodes: graph -> node list
    val edges: graph -> edge list
    
    val edgeCompare: edge * edge -> order
    
    val succ: graph -> node -> node list
    val pred: graph -> node -> node list
    val adj: graph ->  node -> node list
    val eq: node*node -> bool
    
    val newGraph: unit -> graph
    val newNode: graph -> node
    exception GraphEdge
    val mk_edge : graph -> { from: node, to: node } -> unit
    val rm_edge : graph -> { from: node, to: node } -> unit
    
    (* Funciones de impresion *)
    val printNodes : node list -> unit
    val printEdges : edge list -> unit
    val printGraph : graph -> unit
    
    val nodename: node -> string (* for debugging *)
    
end

