

signature tigergraph =
sig
    type node = int
    type edge = {from: node, to: node}
    type graph = {nodes: node list ref, edges: edge list ref}
        
    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list
    val eq: node*node -> bool
    
    val newGraph: unit -> graph
    val newNode: graph -> node
    exception GraphEdge
    val mk_edge : { from: node, to: node } -> unit
    val rm_edge : { from: node, to: node } -> unit
    
(*    structure Table: TABLE *)
(*    sharing type Table.key = node *)
    
    val nodename: node -> string (* for debugging *)
end
(*
structure Flow:
sig
    structure Graph
    datatype flowgraph =
        FGRAPH of {control: graph,
                   def: tigertemp.temp list Table.table,
                   use: tigertemp.temp list Table.table,
                   ismove: bool Table.table}
end*)
                   

(*mk_edge = { from:node, to:node } -> unit
rm_edge = { from: node, to: node } -> unit
nodename: node -> string*)
  
