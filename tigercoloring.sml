structure tigercoloring :> tigercoloring =
struct
    open Array
    open tigerutils
    open tigerliveness
    open tigerflow
    open tigergraph
    open tigertab
    open tigerassem
    open tigerframe
    
    type adjListT    = (tigergraph.node Splayset.set) array
    type wListMoves  = tigergraph.node Splayset.set ref
    type adjSetT     = tigergraph.edge Splayset.set ref
    
    val precolored = listToSet String.compare [rv, ov, "ecx", "ebx", "esi", "edi"]
    val precoloredList = [rv, ov, "ecx", "ebx", "esi", "edi"]
    val initial = ref (Splayset.empty String.compare)
    val iters = ref 0
    
    fun coloring (b, f, firstRun) =
      let
        val newTempsC = ref (Splayset.empty String.compare)
        val (FGRAPH fg, iTable) = instrs2graph b
        val (liveIn, liveOut) = liveAnalysis (FGRAPH fg) 
    
        val uses = List.concat (tabValores (!(#use fg)))
        val defs = List.concat (tabValores (!(#def fg)))
        val totalRegs = unionList (String.compare) uses defs
        val (IGRAPH ig) = newInterGraph()
        val _ = insertNodesLiv (IGRAPH ig) (tigerutils.unionList String.compare totalRegs precoloredList)


        val interNodes = nodes (#graph ig) 
        
        val flowNodes = nodes (#control fg)
        val lenNodes = List.length(interNodes)
        val moveList = ref (array(lenNodes, Splayset.empty (tupleCompare String.compare)))
        val alias = ref (array(lenNodes, ~1))
        val color:((tigertemp.temp, string) tigertab.Tabla) ref = ref (tigertab.fromList [("eax", "eax"), ("ebx", "ebx"),("ecx", "ecx"),("edx", "edx"),("esi", "esi"),("edi", "edi")])

        val spillWorklist = ref (Splayset.empty Int.compare)
        val freezeWorklist = ref (Splayset.empty Int.compare)
        val simplifyWorklist = ref (Splayset.empty Int.compare)

        val worklistMoves = ref (Splayset.empty (tupleCompare String.compare))
        val activeMoves = ref (Splayset.empty (tupleCompare String.compare))
        val coalescedMoves = ref (Splayset.empty (tupleCompare String.compare))
        val frozenMoves = ref (Splayset.empty (tupleCompare String.compare))
        val constrainedMoves = ref (Splayset.empty (tupleCompare String.compare))

        val coalescedNodes = ref (Splayset.empty Int.compare)
        val coloredNodes = ref (Splayset.empty Int.compare)
        val spilledNodes = ref (Splayset.empty Int.compare)
        
        val selectStack: node stack ref = ref tigerutils.emptyStack 
        val adjSet = let 
                        val tupleEdges = List.map (fn {from=f, to=t} => (f, t)) (edges (#graph ig))
                     in
                       ref (Splayset.addList ((Splayset.empty (tupleCompare Int.compare)), tupleEdges))
                     end
                     
        val adjList:(adjListT ref) = ref (array(lenNodes, Splayset.empty Int.compare))
        val degrees:(int array) ref = ref (array(lenNodes, 0))
        val k = 6
        
        (* Funciones para imprimir *)
        fun printIntSet s desc = (print (desc ^ "\n");  Splayset.app (fn x => print (Int.toString(x) ^ "(" ^ (nodeToTemp (IGRAPH ig) x) ^ ")" ^ "\n")) s)
        fun printIntTupleSet s desc = (print (desc ^ "\n"); Splayset.app (fn (x,y) => print ("(" ^ Int.toString(x) ^ ", " ^ Int.toString(y) ^ ")\n")) s)
        fun printStringTupleSet s desc = (print (desc ^ "\n"); Splayset.app (fn (x,y) => print ("(" ^ x ^ ", " ^ y ^ ")\n")) s)   
 
        fun printIntArray a [] = print "Termine \n"
          | printIntArray a (n::ns) = (print (Int.toString(n) ^ " (" ^ (nodeToTemp (IGRAPH ig) n) ^ ")" ^ " -> " ^ Int.toString(Array.sub(a, n)) ^ "\n"); printIntArray a ns)
                
        fun printTodo() = 
          let
            val _ = print "=========================================\n"
            val _ = printIntTupleSet (!adjSet) "adjSet: "
            val _ = printIntSet (!spillWorklist) "spillWorkList: "
            val _ = printIntSet (!freezeWorklist) "freezeWorkList: "
            val _ = printIntSet (!simplifyWorklist) "simplifyWorkList: "
            val _ = printIntSet (!coalescedNodes) "coalescedNodes: "
            val _ = printIntSet (!coloredNodes) "coloredNodes: "
            val _ = printIntSet (!spilledNodes) "spilledNodes: "
            val _ = printStringTupleSet (!worklistMoves) "worklistMoves: "
            val _ = printStringTupleSet (!activeMoves) "activeMoves: "
            val _ = printStringTupleSet (!coalescedMoves) "coalescedMoves: "
            val _ = printStringTupleSet (!frozenMoves) "frozenMoves: "
            val _ = printStringTupleSet (!constrainedMoves) "constrainedMoves: "
            val _ = print ("Degrees: " ^ Int.toString(List.length(interNodes)) ^ "\n")
            val _ = printIntArray (!degrees) interNodes
            val _ = print "\n"
            val _ = print "CODIGO: \n"
            val _ = printInstrList 0 b
            val _ = print "\n=========================================\n"

          in
            ()
          end  
          
        
              
        (* Fin funciones para imprimir *)

        (* Invariantes *)
        fun degreeInv() =
            let
                val cjto1 = Splayset.union(!simplifyWorklist, Splayset.union(!spillWorklist, !freezeWorklist))
                val emptySet = Splayset.empty Int.compare
                val precoloredTemps = Splayset.foldr (fn (x, xs) => Splayset.add(xs, (tempToNode (IGRAPH ig) x))) emptySet precolored
                val cjto2 = Splayset.union(precoloredTemps, cjto1)
                fun printError node g1 g2 = print ("Grado de " ^ Int.toString(node) ^ "(" ^ (nodeToTemp (IGRAPH ig) node) ^ ") " ^ " = " ^ Int.toString(g1) ^ " != " ^ Int.toString(g2) ^ "\n")
            in
                Splayset.foldr (fn (x, xs) => if xs andalso (sub(!degrees, x) = Splayset.numItems(Splayset.intersection(sub(!adjList, x), cjto2))) then true else (printError x (sub(!degrees, x)) (Splayset.numItems(Splayset.intersection(sub(!adjList, x), cjto2))); false)) true cjto1
            end
            
        fun simplifyInv() =
            let
                val cjto1 = Splayset.union(!activeMoves, !worklistMoves)
            in
                Splayset.foldr (fn (x, xs) => xs andalso (sub(!degrees, x) < k andalso Splayset.isEmpty(Splayset.intersection(sub(!moveList, x), cjto1)))) true (!simplifyWorklist)
            end
            
        fun freezeInv() =
            let
                val cjto1 = Splayset.union(!activeMoves, !worklistMoves)
            in
                Splayset.foldr (fn (x, xs) => xs andalso (sub(!degrees, x) < k andalso (not (Splayset.isEmpty(Splayset.intersection(sub(!moveList, x), cjto1)))))) true (!freezeWorklist)
            end
                
        fun spillInv() = Splayset.foldr (fn (x, xs) => xs andalso sub(!degrees, x) >= k) true (!spillWorklist)
            
        (* Fin invariantes *)

        fun addEdge (e as (u, v)) =
            if not(Splayset.member (!adjSet, e)) andalso (u <> v) then
                (adjSet := Splayset.add ((!adjSet), e);
                 adjSet := Splayset.add ((!adjSet), (v, u));
                 (case tabBusca(u, !(#gtemp ig)) of
                        NONE => raise Fail "Error al buscar un nodo en la tabla gtemp"
                      | SOME ut => if not (Splayset.member (precolored, ut)) then
                                     (update(!adjList, u, Splayset.add(sub(!adjList, u), v));
                                      update(!degrees, u, sub(!degrees, u) + 1))
                                  else
                                     ());
                 (case tabBusca(v, !(#gtemp ig)) of
                        NONE => raise Fail "Error al buscar un nodo en la tabla gtemp"
                      | SOME vt => if not (Splayset.member (precolored, vt)) then
                                      (update(!adjList, v, Splayset.add(sub(!adjList, v), u));
                                      update(!degrees, v, sub(!degrees, v) + 1))
                                   else
                                      ()))
             else
                ()
        
        
        fun getColorTmp t =
            case tabBusca(t, !color) of
                    SOME c => c
                  | NONE => "noColor"
                  
        fun getColorNode n =
            let
                val nTmp = nodeToTemp (IGRAPH ig) n
            in
                case tabBusca(nTmp, !color) of
                    SOME c => c
                  | NONE => "noColor"
            end
             
        fun build () =  
            let 
                val l = List.length(b)
                
                val live = ref (case tabBusca((List.hd flowNodes), liveOut) of
                                 NONE => raise Fail ("La instruccion " ^ Int.toString(List.last flowNodes) ^ " no esta en la tabla liveOut")
                               | SOME s' => s' )
                                          
                fun updateLive (n) =
                    let
                        val use = case tabBusca(n, (!(#use fg))) of
                                      NONE => (Splayset.empty String.compare)
                                    | SOME u => listToSet String.compare u
                                    
                        val def = case tabBusca(n, (!(#def fg))) of
                                      NONE => (Splayset.empty String.compare)
                                    | SOME d => listToSet String.compare d
                                    
                        val _ = case tabBusca(n, !iTable) of
                                   SOME (MOVE {assem=a, src="ebp", dst=v}) => ()
                                   | SOME (MOVE {assem=a, src=s, dst="ebp"}) => ()
                                   | SOME (MOVE {assem=a, src=u, dst=v}) => 
                                               let
                                                 val _ = live := Splayset.difference (!live, use)
                                                 val union = Splayset.listItems (Splayset.union(use, def))
                                                 val _ = List.map (fn x => case tabBusca(x, !(#tnode ig)) of
                                                                               NONE => raise Fail ("El temp " ^ x ^ " no esta en la tabla tnode")
                                                                             | SOME xi => update(!moveList, xi, Splayset.add(sub(!moveList, xi), (u, v) ))) union
                                                                                
                                                 val _ = worklistMoves := Splayset.add(!worklistMoves, (u,v))

                                               in
                                                 ()
                                               end 
                                    | _ => ()
                        val _ = live := Splayset.union(!live, def)
                        val _ = List.map (fn d => case tabBusca(d, !(#tnode ig)) of
                                        NONE => raise Fail "El temp no esta en la tabla tnode (2)"
                                      | SOME di => List.map (fn l => case tabBusca(l, !(#tnode ig)) of
                                                    NONE => raise Fail "El temp no esta en la tabla tnode (3)"
                                                  | SOME li => (addEdge((li, di)))) (Splayset.listItems (!live))) (Splayset.listItems def) 
                        val _ = live := Splayset.union(use, Splayset.difference(!live, def))
                    in
                        ()
                    end
          
             in 
             List.map updateLive flowNodes
             end

        fun nodeMoves n = Splayset.intersection(sub(!moveList, n), (Splayset.union(!activeMoves, !worklistMoves)))
        fun moveRelated n = not (Splayset.isEmpty (nodeMoves n))
        
        (* 6 es el K, y hay que pasarle una lista de todos los temporales (el initial) *)
        fun makeWorklist () = 
          let
            fun makeWorklist' m =
                let 
                   val n = tempToNode (IGRAPH ig) m
                in
                   if sub(!degrees, n) >= k then
                      spillWorklist := Splayset.add(!spillWorklist, n)
                   else if moveRelated(n) then
                      freezeWorklist := Splayset.add(!freezeWorklist, n)
                   else
                      simplifyWorklist := Splayset.add(!simplifyWorklist, n)
                end
          in
             Splayset.app makeWorklist' (!initial)
          end



        fun adjacent n = Splayset.difference(sub(!adjList, n), Splayset.union(stackToSet Int.compare (!(selectStack)), !coalescedNodes))
        
        fun printAdjacentes n =
            (print("Adjacentes de " ^ Int.toString(n) ^ "\n");
            Splayset.app (fn x => print(Int.toString(x) ^"\n")) (adjacent n))

        fun enableMoves nodes = 
            let
                fun f x = (List.map (fn m => if Splayset.member(!activeMoves, m) then
                                                (activeMoves := Splayset.delete(!activeMoves, m);
                                                 worklistMoves := Splayset.add(!worklistMoves, m))
                                          else
                                                ()) (Splayset.listItems (nodeMoves x)); ())
            in
                Splayset.app f nodes
            end
                                     

        fun decrementDegree n =
            let 
                val d = sub(!degrees, n)
            in
                if (d = 0) then () 
                else
                    (update(!degrees, n, d - 1);
                     
                     if d = k then
                        (enableMoves (Splayset.add((adjacent n), n));
                         spillWorklist := Splayset.difference(!spillWorklist, Splayset.singleton Int.compare n);
                         if moveRelated n then
                            freezeWorklist := Splayset.add(!freezeWorklist, n)
                         else
                            simplifyWorklist := Splayset.add(!simplifyWorklist, n))
                     else
                         ())
            end
          
        fun simplify () =
            let
              val n = List.hd (Splayset.listItems (!simplifyWorklist))
            in
             (simplifyWorklist := Splayset.delete(!simplifyWorklist, n);
              tigerutils.push n selectStack;
              Splayset.app (fn x => decrementDegree x)
                                      (adjacent n))
            end

        (* Coalesced *)

        fun getAlias n =
            if Splayset.member(!coalescedNodes, n) then
               getAlias (sub(!alias, n))
            else
               n

        fun combine u v =
           (if Splayset.member(!freezeWorklist, v) then
              freezeWorklist := Splayset.delete(!freezeWorklist, v)
            else
              spillWorklist := Splayset.difference(!spillWorklist, Splayset.singleton Int.compare v);
            coalescedNodes := Splayset.add(!coalescedNodes, v);
            update(!alias, v, u);
            update(!moveList, u, Splayset.union(sub(!moveList, u), sub(!moveList, v)));
            enableMoves(Splayset.singleton Int.compare v);
            Splayset.app (fn t => (addEdge((t, u)); decrementDegree t)) (adjacent v);
            if sub(!degrees, u) >= k andalso Splayset.member(!freezeWorklist, u) then
               (freezeWorklist := Splayset.delete(!freezeWorklist, u);
                spillWorklist := Splayset.add(!spillWorklist, u))
            else
                ())
            
            
        fun addWorkList u =
           let 
               val unode = case tabBusca(u, !(#tnode ig)) of
                               NONE => raise Fail "No se encontro el nodo (addWorkList)"
                             | SOME s => s
           in
               if not(Splayset.member(precolored, u)) andalso not (moveRelated unode) andalso sub(!degrees, unode) < k then
                   (freezeWorklist := Splayset.difference(!freezeWorklist, Splayset.singleton Int.compare unode);
                   (simplifyWorklist := Splayset.add(!simplifyWorklist, unode)))
               else
                  ()
           end
           
        fun ok (t, r) =
          let
             val tnod = case tabBusca(t, !(#tnode ig)) of
                           NONE => raise Fail "No se encontro el nodo (ok - t)"
                         | SOME n => n
                         
             val rnod = case tabBusca(t, !(#tnode ig)) of
                           NONE => raise Fail "No se encontro el nodo (ok - r)"
                         | SOME n => n
          in
             sub(!degrees, tnod) < k orelse Splayset.member(precolored, t) orelse Splayset.member(!adjSet, (tnod, rnod))
          end
          
        fun conservative cn =
            let 
                val k' = ref 0
                val _ = Splayset.app (fn n => if sub(!degrees, n) >= k then k' := !k' + 1 else ()) cn
             in
                !k' < k
             end
             
        fun coalesce () =
            let
               fun coalesce' (m as (x, y)) =
                   let
                      val x' = nodeToTemp (IGRAPH ig) (getAlias(tempToNode (IGRAPH ig) x))
                      val y' = nodeToTemp (IGRAPH ig) (getAlias(tempToNode (IGRAPH ig) y))
                      val (u, v) = if Splayset.member(precolored, y') then
                                      (y', x')
                                   else
                                      (x', y')

                      val (uNode, vNode) = (tempToNode (IGRAPH ig) u, tempToNode (IGRAPH ig) v)

                      val adjacentTmp = Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (adjacent vNode)
                      val checkForall = Splayset.foldr (fn (t, xs) => (ok (t, u)) andalso xs) true adjacentTmp
                      
                   in
                     (worklistMoves := Splayset.difference(!worklistMoves, Splayset.singleton (tupleCompare String.compare) m);
                      if u = v then
                        (coalescedMoves := Splayset.add(!coalescedMoves, m);      
                         addWorkList u)
                      else if Splayset.member(precolored, v) orelse Splayset.member(!adjSet, (uNode, vNode)) then
                         (constrainedMoves := Splayset.add(!constrainedMoves, m);
                          (addWorkList u);
                          (addWorkList v))
                      else if ((Splayset.member(precolored, u) andalso checkForall)) orelse 
                              ((not(Splayset.member(precolored, u)) andalso conservative(Splayset.union(adjacent uNode, adjacent vNode)))) then
                                (coalescedMoves := Splayset.add(!coalescedMoves, m);
                                (combine uNode vNode);
                                (addWorkList u))
                      else
                        activeMoves := Splayset.add(!activeMoves, m))                                         
                   end 
            in
                Splayset.app coalesce' (!worklistMoves)
            end

        fun freezeMoves u =
          let
            fun processNodeMove (m as (x,y)) =
              let
                val yAlias = getAlias (tempToNode (IGRAPH ig) y)
                val xAlias = getAlias (tempToNode (IGRAPH ig) x)
                val uAlias = getAlias u		
                val v = if yAlias = uAlias then xAlias
                        else yAlias
              in
                (activeMoves := Splayset.difference(!activeMoves, Splayset.singleton (tupleCompare String.compare) m);
                 frozenMoves := Splayset.add(!frozenMoves, m);
                 if (Splayset.isEmpty (nodeMoves v)) andalso sub(!degrees, v) < k then
                    (freezeWorklist := Splayset.difference(!freezeWorklist, Splayset.singleton Int.compare v);
                    simplifyWorklist := Splayset.add(!simplifyWorklist, v))
                 else
                   ())
              end
          in
            Splayset.app processNodeMove (nodeMoves u)
          end

        fun freeze () =
          let
            fun freeze' u =  (freezeWorklist := Splayset.difference(!freezeWorklist, Splayset.singleton Int.compare u);
                             simplifyWorklist := Splayset.add(!simplifyWorklist, u);
                             freezeMoves u)
                    
          in
            Splayset.app freeze' (!freezeWorklist)
          end 

        fun selectSpill () =
          let 
            fun spillPriority (n) =
                let
                    val nDef = List.foldr (fn ((x, y), xs) => if tigerutils.inList (nodeToTemp (IGRAPH ig) n)  y then 1.0 + xs else xs) 0.0 (tigertab.tabAList (!(#def fg)))
                    val nUse = List.foldr (fn ((x, y), xs) => if tigerutils.inList (nodeToTemp (IGRAPH ig) n) y then 1.0 + xs else xs) 0.0 (tigertab.tabAList (!(#use fg)))
                    val nAdj = Splayset.numItems(sub(!adjList, n))
                in
                    if tigerutils.inList (nodeToTemp (IGRAPH ig) n) ["eax", "ebx", "ecx", "edx", "esi", "edi"] then 9999999.0 else (nDef + nUse) / real(nAdj)
                end
            
            fun minElem [] min node = node
              | minElem ((x, y)::xs) min node =
                    if x < min then minElem xs x y 
                    else minElem xs min node
            
            val priorities = List.map (fn x => (spillPriority(x), x)) (Splayset.listItems (!spillWorklist))
            val m = minElem priorities 9999.0 ~1
            
          in
             (spillWorklist := Splayset.difference(!spillWorklist, Splayset.singleton Int.compare m);
             simplifyWorklist := Splayset.add(!simplifyWorklist, m);
             freezeMoves m)
          end


        fun assignColors () =
          let 
            val okColors = ref (["eax","ebx","ecx","edx","edi","esi"])
            val n = ref ~1 (* Valor dummy para usar en el pop *)
            fun processAdjList n = Splayset.app (fn w => let
                                                            val wAlias = getAlias w
                                                            val wtmp = nodeToTemp (IGRAPH ig) wAlias 
                                                            val coloredNodesTmp = Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (!coloredNodes)
                                                            val wAliasColor = getColorTmp wtmp
                                                          in
                                                            if Splayset.member(Splayset.union(coloredNodesTmp, precolored), wtmp) then
                                                                okColors := List.filter (fn x => x <> wAliasColor) (!okColors)
                                                            else 
                                                               ()
                                                          end) (sub(!adjList, n))
          
          
          in
            (while (not(isEmptyStack selectStack)) do
                (n := pop selectStack;
                 okColors := ["eax","ebx","ecx","edx","edi","esi"];
                 processAdjList (!n);
                 if List.null(!okColors) then
                      spilledNodes := Splayset.add(!spilledNodes, (!n))
                 else
                    (coloredNodes := Splayset.add(!coloredNodes, (!n));
                     color := tabInserta(nodeToTemp (IGRAPH ig) (!n), List.hd (!okColors), !color);
                     okColors := List.tl (!okColors)));
               Splayset.app (fn m => let
                                        val mAliasTmp = nodeToTemp (IGRAPH ig) (getAlias m)
                                        val mTmp = nodeToTemp (IGRAPH ig) (m)
                                        val mAliasColor = getColorTmp mAliasTmp
                                     in
                                        color := tabInserta(mTmp, mAliasColor, !color)
                                     end) (!coalescedNodes))
          end    
      
       
        fun rewriteProgram () =
            let
                val spilledNodesTmp = ref (Splayset.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) (!spilledNodes)) 
                fun getNewAlloc () = case (allocLocal f true) of
                                            InFrame m' => if m' < 0 then "-"^Int.toString(Int.abs(m'))
                                                          else Int.toString(Int.abs(m'))
                                            | _ => raise Fail "En true esto no deberia pasar...."
                val memLocsTab = tigertab.fromList ((List.map (fn s => (s, getNewAlloc()))) (Splayset.listItems (!spilledNodesTmp)))

                
                fun makeFetch oldT newT= 
                    let
                        val m = case tigertab.tabBusca(oldT, memLocsTab) of
                                     SOME mem => mem
                                    | NONE => raise Fail("No hay memoria alocada para el temporal " ^ oldT ^ "\n")
                    in
                        OPER {assem="movl " ^ m ^ "(%ebp), `d0\n" , dst=[newT], src=[], jump=NONE}
                    end
                    
                fun makeStore oldT newT= 
                    let
                        val m = case tigertab.tabBusca(oldT, memLocsTab) of
                                     SOME mem => mem
                                    | NONE => raise Fail("No hay memoria alocada para el temporal " ^ oldT ^ "\n")
                    in
                        OPER {assem="movl `s0, "^ m ^ "(%ebp)\n", dst=[], src=[newT], jump=NONE}
                    end

                fun rewriteInstruction (i as LABEL l) = [i]
                  | rewriteInstruction i = 
                    let 
                       val spillUses = Splayset.listItems(Splayset.intersection(!spilledNodesTmp, Splayset.addList (Splayset.empty String.compare, tigerassem.src2List i)))
                       val spillDefs = Splayset.listItems(Splayset.intersection(!spilledNodesTmp, Splayset.addList (Splayset.empty String.compare, tigerassem.dst2List i)))
                       val newtempsTab = tigertab.fromList (List.map (fn t => let
                                                                                 val t' = tigertemp.newtemp()
                                                                                 val _ = newTempsC := Splayset.add(!newTempsC, t')
                                                                              in (t, t') end) (tigerutils.unionList (String.compare) spillUses spillDefs))
                       fun newTemps x = case tigertab.tabBusca (x, newtempsTab) of
                                            SOME t => t
                                          | NONE => x

                       val fetches = List.map (fn t => makeFetch t (newTemps t)) spillUses
                       val stores = List.map (fn t => makeStore t (newTemps t)) spillDefs                   
                       
                       
                       val newInstr = case i of 
                                        OPER {assem, dst, src, jump} => OPER{assem=assem, dst = List.map newTemps dst, src = List.map newTemps src, jump = jump}
                                      | MOVE {assem, dst, src} => MOVE{assem = assem, dst = newTemps dst, src = newTemps src}
                                      | x => x    
                    in
                        (fetches @ [newInstr] @ stores)                     
                    end
                val rewritedInstrs = List.concat (List.map rewriteInstruction b)
                
            in
              rewritedInstrs
            end
                

      fun repeat () =
       if not(Splayset.isEmpty(!simplifyWorklist)) then simplify()
       else if not(Splayset.isEmpty(!worklistMoves)) then coalesce()
       else if not(Splayset.isEmpty(!freezeWorklist)) then freeze()
       else if not(Splayset.isEmpty(!spillWorklist)) then selectSpill()
       else ()         


      in
        (build ();
         if firstRun then
             initial := let 
                           val nodesIG = List.foldr (fn (x, xs) => Splayset.add(xs, nodeToTemp (IGRAPH ig) x)) (Splayset.empty String.compare) interNodes
                         in
                           Splayset.difference(nodesIG, precolored)
                         end
         else
            ();
         makeWorklist(); 
         if degreeInv() then () else raise Fail "Error en degreeInv";
         if simplifyInv() then () else raise Fail "Error en degreeInv";
         if freezeInv() then () else raise Fail "Error en degreeInv";
         if spillInv() then () else raise Fail "Error en degreeInv";
         
         repeat();     
         while not(Splayset.isEmpty(!simplifyWorklist) andalso Splayset.isEmpty(!worklistMoves) andalso
                   Splayset.isEmpty(!freezeWorklist) andalso Splayset.isEmpty(!spillWorklist)) do
               repeat();
         assignColors();
         if not(Splayset.isEmpty(!spilledNodes)) then
            if (!iters)<=1  then
                (iters := (!iters) + 0;
                coloring(rewriteProgram(), f, true))
            else (b, f, !color)
         else
            (b, f, !color))

         

      end


end
