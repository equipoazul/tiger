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
                   
    fun initList ns l = List.map (fn x => l := tabInserta(x, Splayset.empty String.compare, !l)) ns
    
    fun getSuccIn n = let
                          val succs = succ n
                      in
                         List.map (fn x => case tabBusca(x, !liveIn) of
                            NONE => raise Fail "Error al buscar un nodo en la tabla liveIn (1)"
                          | SOME c => c) succs
                      end
    
    fun printSet s = List.foldr (fn (x, xs) => x ^ "," ^ xs) "" (Splayset.listItems s)
    fun printLiveT t = map (fn (x,y) => print ( (Int.toString x) ^ " -> " ^ (printSet y) ^ "\n") ) (tigertab.tabAList t)
    
    fun liveAnalysis (FGRAPH fg, ns) = 
      let
        fun liveAnalysis' ((FGRAPH fg), []) = 
             let
               val _ = print "Live In\n"
               val _ = printLiveT (!(liveIn))
               val _ = print "Live Out\n"
               val _ = printLiveT (!(liveOut))
             in
               ()
             end
            | liveAnalysis' ((FGRAPH fg), (n::ns)) =
              let 
                fun listToSet l = 
                  let
                    val emptySet = Splayset.empty String.compare
                  in
                    Splayset.addList (emptySet, l)
                  end            
                  
                val inn' = ref (Splayset.empty String.compare)
                val out' = ref (Splayset.empty String.compare)
                val inn = ref (Splayset.empty String.compare)  
                val out = ref (Splayset.empty String.compare)  
                (* val inn = case tabBusca(n, !liveIn) of 
                                      NONE => raise Fail "Error al buscar un nodo en la tabla liveIn (2)"
                                    | SOME c => ref c
                val out = case tabBusca(n, !liveOut) of 
                                      NONE => raise Fail "Error al buscar un nodo en la tabla liveOut"
                                    | SOME c => ref c*)
                val use = case tabBusca(n, !(#use fg)) of 
                                      NONE => Splayset.empty String.compare 
                                    | SOME tList => listToSet tList
                val def = case tabBusca(n, !(#def fg)) of 
                                      NONE => Splayset.empty String.compare 
                                    | SOME tList => listToSet tList

                val _ = inn := Splayset.union (use, (Splayset.difference (!out, def)));
                val _ = out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n)
                
                val _ = while (not (Splayset.equal (!inn, !inn') andalso Splayset.equal (!out, !out'))) do
                           (inn' := !inn;
                            out' := !out;
                            inn := Splayset.union (use, (Splayset.difference (!out, def)));
                            out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n))
                val _ = liveIn := tabRInserta(n, !inn, !liveIn)
                val _ = liveOut := tabRInserta(n, !out, !liveOut)
                in
                    liveAnalysis' ((FGRAPH fg), ns)
                end
      in
          (initList (nodes (#control fg)) liveIn;
           initList (nodes (#control fg)) liveOut;
           printGraph (#control fg); 
           liveAnalysis' (FGRAPH fg, ns))
      end

end

