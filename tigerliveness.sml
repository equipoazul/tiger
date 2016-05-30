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
        fun initTab ns = let
                             val t = ref (tabNueva():liveSet)
                             val _ = List.map (fn x => t := tabInserta(x, Splayset.empty String.compare, !t)) ns
                          in
                              t
                          end
        fun initList ns = let
                             val l = ref (List.map (fn x => Splayset.empty String.compare) ns)
                          in
                              l
                          end
        val ns = (nodes (#control fg))
        val liveIn = initTab ns 
        val liveOut = initTab ns
        val inn = initList ns
        val out = initList ns
        val inn' = initList ns
        val out' = initList ns

        fun getSuccIn n = let
                          val succs = succ (#control fg) n
                      in
                         List.map (fn x => case tabBusca(x, !liveIn) of
                                                  NONE => raise Fail "Error al buscar un nodo en la tabla liveIn (1)"
                                                | SOME c => c) succs
                      end
                      
        fun foreach ((FGRAPH fg), n) = 
            let       
                
              val use = case tabBusca(n, !(#use fg)) of
                                    NONE => Splayset.empty String.compare
                                  | SOME tList => tigerutils.listToSet String.compare tList
              val def = case tabBusca(n, !(#def fg)) of
                                    NONE => Splayset.empty String.compare 
                                  | SOME tList => tigerutils.listToSet String.compare tList
              val _ = inn' := setNthList n (!inn') (List.nth(!inn, n))
              val _ = out' := setNthList n (!out') (List.nth(!out, n))
              val newElemInn = Splayset.union (use, (Splayset.difference (List.nth(!out, n), def)))
              val _ = inn := setNthList n (!inn) newElemInn
              val newElemOut = List.foldr Splayset.union (Splayset.empty String.compare) (getSuccIn n)
              val _ = out := setNthList n (!out) newElemOut
            in
                (liveIn := tabRInserta(n, List.nth(!inn, n), !liveIn);
                liveOut := tabRInserta(n, List.nth(!out, n), !liveOut))
            end
            
        fun repeat() = List.map (fn x => foreach ((FGRAPH fg), x)) ns
        
        fun evalCondition() = 
            let
               val e1 = foldr (fn ((x, y), xs) => (Splayset.equal (x, y)) andalso xs) true (ListPair.zip(!inn', !inn))
               val e2 = foldr (fn ((x, y), xs) => (Splayset.equal (x, y)) andalso xs) true (ListPair.zip(!out', !out))
            in
               e1 andalso e2
            end
        
        val _ = repeat()
        val _ = while not (evalCondition()) do
                  repeat()
        
     in
        (*(print "Live In\n";
         printLiveT (!liveIn);
         print "Live Out\n";
         printLiveT (!liveOut);*)
        (!liveIn, !liveOut)
      end

end

