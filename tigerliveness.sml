structure tigerliveness :> tigerliveness =
struct

    open tigergraph
    open tigertemp
    
    datatype igraph =
       IGRAPH of {graph: tigergraph.graph,
                  tnode: tigertemp.temp -> tigergraph.node,
                  gtemp: tigergraph.node -> tigertemp.temp,
                  moves: (tigergraph.node * tigergraph.node) list}
                   
                                  
end

