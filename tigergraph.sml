structure tigergraph :> tigergraph =
struct
    open tigerutils
    
    exception GraphEdge
    type node = int
    type edge = {from: node, to: node}
    type graph = {nodes: node list ref, edges: edge list ref}
        
    fun newGraph() = {nodes = ref [], edges = ref []} : graph
    
    val gr = newGraph ()

    fun nodes g = (!(#nodes (g:graph)))
    fun edges g = (!(#edges (g:graph)))
    
    fun succ n = let
                     val edges = List.filter (fn x => (#from x) = n) (!(#edges gr))
                     val nodes = List.map (fn {from=inode, to=onode} => onode) edges
                 in
                     nodes
                 end
        
    fun pred n = let
                     val edges = List.filter (fn x => (#to x) = n) (!(#edges gr))
                     val nodes = List.map (fn {from=inode, to=onode} => inode) edges
                 in
                     nodes
                 end

    fun adj n = tigerutils.unionList (pred n) (succ n)

    fun eq (n,m) = n = m
             
    fun newNode {nodes = vs , edges = _}  = let 
                                                val ret = (case !vs of
                                                             [] => 0
                                                           | ws => List.hd(ws) + 1)
                                                val _ = vs := (ret :: (!vs))
                                            in
                                                ret
                                            end
                          
    fun mk_edge e = let
                        val edges = (#edges gr)
                        val _ = edges := ( e :: (!edges) )
                    in
                        ()
                    end
                            
    fun rm_edge e = let
                        val edges = (#edges gr)
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
                   
                   
    
end 

