structure tigergraph :> tigergraph =
struct

    val gr = {nodes = [], edges = []}

    fun nodes {nodes = vs, edges = _} = vs

    fun succ n = List.filter (fn x => (#from x) = n) (#edges gr)    
        
    fun pred n = List.filter (fn x => (#to x) = n) (#edges gr)

    fun adj n = (pred n) @ (succ n)

    fun eq (n,m) = n = m
             
    fun newGraph = {nodes = [], edges = []}

    fun newNode {nodes = vs, edges = _}  = let 
                                                val ret = (case !vs of
                                                             [] => 0
                                                           | vs => List.hd(!vs) + 1)
                                                val _ = vs := (n :: (!vs)) ref
                                            in
                                                ret
                                            end
                                                
                      
(*    val gr = newGraph() *)
    
end
