structure tigergraph :> tigergraph =
struct
    open tigerutils
    
    exception GraphEdge
    type node = int
    type edge = {from: node, to: node}
    type graph = {nodes: node list ref, edges: edge list ref}
        
    fun newGraph() = {nodes = ref [], edges = ref []} : graph

    fun nodes g = (!(#nodes (g:graph)))
    fun edges g = (!(#edges (g:graph)))
    
    fun edgeCompare ({from=f, to=t}, {from=f', to=t'}) =
        if f = f' andalso t = t' then EQUAL
        else LESS
    
    fun succ (g as {edges, nodes}) n = let
                                         val eds = List.filter (fn x => (#from x) = n) (!edges)
                                         val nods = List.map (fn {from=inode, to=onode} => onode) eds
                                       in
                                         nods
                                       end
        
    fun pred (g as {edges, nodes}) n = let
                                         val edgs = List.filter (fn x => (#to x) = n) (!edges)
                                         val nods = List.map (fn {from=inode, to=onode} => inode) edgs
                                       in
                                         nods
                                       end

    fun adj g n = tigerutils.unionList (Int.compare) (pred g n) (succ g n)

    fun eq (n,m) = n = m
             
    fun newNode {nodes = vs , edges = _}  = let 
                                                val ret = (case !vs of
                                                             [] => 0
                                                           | ws => List.hd(ws) + 1)
                                                val _ = vs := (ret :: (!vs))
                                            in
                                                ret
                                            end
                          
    fun mk_edge (g as {edges, nodes}) e = let
                                            val _ = edges := ( e :: (!edges) )
                                          in
                                            ()
                                          end
                            
    fun rm_edge (g as {edges, nodes}) e = let
                                            val _ = edges := List.filter (fn e1 => e1 <> e ) (!edges) 
                                          in
                                            ()
                                          end
                         
    fun nodename n = Int.toString(n)
    
    fun printNodes g = let 
                        val _ = print "["
                        val _ = map (fn x => (print (nodename x); print (", ")) ) g
                        val _ = print "]\n"
                       in
                         ()
                       end
                       
    fun printEdges g = let 
                            val _ = print "["
                            val _ = map (fn x => (print("<"); print(nodename (#from x)); print (", "); print(nodename (#to x)); print (">")) ) (g: edge list)
                            val _ = print "]\n"
                   in
                     ()
                   end
                   
    fun printGraph g = (printNodes (nodes g); printEdges (edges g))
                   
                   
    
end 

