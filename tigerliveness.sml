structure tigerliveness :> tigerliveness =
struct

    open tigergraph
    open tigertemp
    open tigertab
    open tigerflow 
    open tigerutils    
    
    (*type liveSet = node Splayset.set * tigertemp.temp list
    type liveMap = (tigergraph.node, liveSet) tigertab.Tabla*)
    type liveSet = (tigergraph.node, string Splayset.set) tigertab.Tabla
    
    datatype igraph =
       IGRAPH of {graph: tigergraph.graph,
                  tnode: (tigertemp.temp, tigergraph.node) tigertab.Tabla ref,
                  gtemp: (tigergraph.node, tigertemp.temp) tigertab.Tabla ref,
                  moves: (tigergraph.node * tigergraph.node) list ref}   
    
    fun newInterGraph() =
            let
                val g = newGraph()
                val tn = ref (tabNueva())
                val gt = ref (tabNueva())
                val m = ref []
                val ig = IGRAPH {
                            graph = g,
                            tnode = tn,
                            gtemp = gt,
                            moves = m}
            in 
                ig
            end
                
    
    fun insertNodesLiv _ [] = ()
      | insertNodesLiv (IGRAPH ig) (n::ns) =
      let
         val m = newNode (#graph ig)
         val _ = (#tnode ig) := tabInserta(n, m, !(#tnode ig))
         val _ = (#gtemp ig) := tabInserta(m, n, !(#gtemp ig))
      in
         insertNodesLiv (IGRAPH ig) ns
      end
    
    fun printSet s = List.foldr (fn (x, xs) => x ^ "," ^ xs) "" (Splayset.listItems s)
    fun printLiveT t = map (fn (x,y) => print ( (Int.toString x) ^ " -> " ^ (printSet y) ^ "\n") ) (tigertab.tabAList t)

    fun nodeToTemp (IGRAPH ig) x = case tabBusca(x, !(#gtemp ig)) of
                                                     NONE => raise Fail ("No se encontro el nodo " ^ Int.toString(x) ^ " (nodToTemp)")
                                                   | SOME n => n
                             
    fun tempToNode (IGRAPH ig) x = case tabBusca(x, !(#tnode ig)) of
                                                     NONE => raise Fail ("No se encontro el nodo " ^ x ^ " (tempToNode)")
                                                   | SOME n => n

    fun liveAnalysis (FGRAPH fg) = 
      let
        fun initList ns = let
                               val t = ref (tabNueva():liveSet)
                               val _ = List.map (fn x => t := tabInserta(x, Splayset.empty String.compare, !t)) ns
                            in
                                t
                            end
        val ns = (nodes (#control fg))
        val liveIn = initList ns 
        val liveOut = initList ns

        fun getSuccIn n = let
                          val succs = succ (#control fg) n
                      in
                         List.map (fn x => case tabBusca(x, !liveIn) of
                            NONE => raise Fail "Error al buscar un nodo en la tabla liveIn (1)"
                          | SOME c => c) succs 
                      end
                      
        fun liveAnalysis' ((FGRAPH fg), []) = 
             let
             (*
               val _ = print "Live In\n"
               val _ = printLiveT (!liveIn)
               val _ = print "Live Out\n"
               val _ = printLiveT (!liveOut)*)
               val _ = ()
             in
               (!liveIn, !liveOut)
             end
            | liveAnalysis' ((FGRAPH fg), (n::ns)) =
              let       
                  
                val inn' = ref (Splayset.empty String.compare)
                val out' = ref (Splayset.empty String.compare)
                val inn = ref (Splayset.empty String.compare)  
                val out = ref (Splayset.empty String.compare)  
                val use = case tabBusca(n, !(#use fg)) of
                                      NONE => Splayset.empty String.compare
                                    | SOME tList => tigerutils.listToSet String.compare tList
                val def = case tabBusca(n, !(#def fg)) of
                                      NONE => Splayset.empty String.compare 
                                    | SOME tList => tigerutils.listToSet String.compare tList

                val _ = inn := Splayset.union (use, (Splayset.difference (!out, def)));
                val _ = out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n)

                fun repeat() =
                           (inn' := !inn;
                            out' := !out;
                            inn := Splayset.union (use, (Splayset.difference (!out, def)));
                            out := List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n))
                val _ = repeat()
                val _ = while (not (Splayset.equal (!inn, !inn') andalso Splayset.equal (!out, !out'))) do
                           repeat()
                val _ = liveIn := tabRInserta(n, !inn, !liveIn)
                val _ = liveOut := tabRInserta(n, !out, !liveOut)
                in
                    liveAnalysis' ((FGRAPH fg), ns)
                end
      in
           liveAnalysis' (FGRAPH fg, ns)
      end

end

