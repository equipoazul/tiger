structure tigerliveness :> tigerliveness =
struct

    open tigergraph
    open tigertemp
    open tigertab
    open tigerflow 
    
    (*type liveSet = node Splayset.set * tigertemp.temp list
    type liveMap = (tigergraph.node, liveSet) tigertab.Tabla*)
    type liveSet = (tigergraph.node, string Splayset.set) tigertab.Tabla

    val liveIn = ref (tabNueva())
    val liveOut = ref (tabNueva())

    datatype igraph =
       IGRAPH of {graph: tigergraph.graph,
                  tnode: tigertemp.temp -> tigergraph.node,
                  gtemp: tigergraph.node -> tigertemp.temp,
                  moves: (tigergraph.node * tigergraph.node) list}
                   
    fun initList ns = List.foldr (fn (x, xs) => tabInserta(x, Splayset.empty String.compare, !liveIn)) ns
    
    fun getSuccIn n = let
                          val succs = succ n
                      in
                         List.map (fn x => case tabBusca(x, !liveIn) of
                            NONE => raise Fail "Error al buscar un nodo en la tabla liveIn"
                          | SOME c => c) succs
                      end
                                  
    fun liveAnalysis (FGRAPH fg) [] = ()
        | liveAnalysis (FGRAPH fg) (n::ns) =
          let 
            fun listToSet l = 
              let
                val emptySet = Splayset.empty String.compare
              in
                Splayset.addList (emptySet, l)
              end            
              
            val inn' = Splayset.empty String.compare
            val out' = Splayset.empty String.compare 
            val inn = case tabBusca(n, !liveIn) of 
                                  NONE => raise Fail "Error al buscar un nodo en la tabla liveIn"
                                | SOME c => ref c
            val out = case tabBusca(n, !liveOut) of 
                                  NONE => raise Fail "Error al buscar un nodo en la tabla liveOut"
                                | SOME c => ref c
            val use = case tabBusca(n, !(#use fg)) of 
                                  NONE => raise Fail "Error al buscar un nodo en el grafo de inferencia (use)"
                                | SOME tList => listToSet tList
            val def = case tabBusca(n, !(#def fg)) of 
                                  NONE => raise Fail "Error al buscar un nodo en el grafo de inferencia (def)"
                                | SOME tList => listToSet tList

            val _ = while (not (Splayset.equal (!inn, inn') andalso Splayset.equal (!out, out'))) do
                        (inn := inn';
                        out := out';
                        inn := Splayset.union (use, (Splayset.difference (!out, def)));
                        out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n))
            in
                liveAnalysis (FGRAPH fg) ns
            end

end

