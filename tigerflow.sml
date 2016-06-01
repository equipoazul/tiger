structure tigerflow :> tigerflow =
struct

    open tigergraph
    open tigertab
    open tigerassem
  
    type instrTable = ((tigergraph.node, tigerassem.instr) tigertab.Tabla) ref
    type tempTable  = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    type boolTable  = (tigergraph.node, bool) tigertab.Tabla

    datatype flowgraph =
        FGRAPH of {control: tigergraph.graph,
                   def: tempTable ref,
                   use: tempTable ref,
                   ismove: boolTable ref}
                   
    fun newFlowGraph() =
            let
                val g = tigergraph.newGraph()
                val bTable = ref (tabNueva())
                val useTable = ref (tabNueva())
                val defTable = ref (tabNueva())
                val fg = FGRAPH {control = g,
                                 def = defTable, 
                                 use = useTable,
                                 ismove = bTable}
            in
                fg
            end
               
    (*
    fun getNode i = case tabBusca(i, !iTable) of
                             SOME n => (print "Holaaaa\n"; n)
                           | _ => raise Fail "Error al buscar instrucciones en la tabla 1"
                           
    *)                      
    
    fun printGraphFlow ((FGRAPH f): flowgraph) =
        let
            val g = #control(f)
            fun printTab t s = print(s ^ ":\n" ^ (String.concatWith "\n" (List.map (fn (n, ts) => tigergraph.nodename n ^ " -> " ^ (String.concatWith ", " ts) ^ " ") (tigertab.tabAList t))) ^ "\n")
        in
            print("FLOWGRAPH: \n");
            tigergraph.printGraph g;
            printTab (!(#use(f))) "Use";
            printTab (!(#def(f))) "Def"
        end
        
                                  
    fun instrs2graph l = 
      let 
        val iTable:instrTable = ref (tabNueva())
        val (FGRAPH fg) = newFlowGraph()                                  
        
        fun createNodes [] labelList = labelList
          | createNodes (x::xs) labelList = 
                let
                    val n = newNode (#control fg)
                    val _ = iTable := tabInserta(n, x, !iTable)        
                  in
                     case x of
                        LABEL {lab=l, ...} => createNodes xs ((l, n)::labelList)
                      | _ => createNodes xs labelList
                  end  

        val labelList = createNodes l []
        
        fun admittedRegs r = (tigerutils.inList r ["eax", "ebx", "ecx", "edx", "esi", "edi"]) orelse (String.isPrefix "T" r)
                      
        fun getLabelNode [] l' = raise Fail ("Error al buscar el label " ^ l' ^ " en la lista de labels.")
          | getLabelNode ((l, n)::xs) l' = if l = l' then n
                                         else getLabelNode xs l'             
                                                               
        fun instrs2graph' [] n (FGRAPH fg) labelList = fg
           | instrs2graph' (x::xs) n (FGRAPH fg) labelList = 
             let
                (*val _ = print("INSTR2GRAPH NODO " ^ Int.toString(n) ^ "\n")*)
                val (n2, lastNode) = if xs <> [] then (n + 1, false)
                                     else (~1, true)
                val _ = case x of
                          OPER {assem = s,
                                dst = dst,
                                src = src,
                                jump = jmp} => let  val validDst = List.filter admittedRegs dst
                                                    val validSrc = List.filter admittedRegs src
                                                    (*val _ = (print("DST: "); List.map (fn x => print(x ^ ", ")) dst; print("\n"))
                                                    val _ = (print("SRC: "); List.map (fn x => print(x ^ ", ")) src; print("\n"))*)
                                                    (*val _ = if tigerutils.inList "T208" src then print "ALEEEEEEEEEEEEEEEEEEEEEEEEJOO \n" else print "VALENTIIIIIIIIIIIINA \n" *)
                                                    val _ = if not (List.null validDst) then (*(print("Inserte "); List.map (fn x => print(x ^ ", ")) validDst; print(" en dst\n");*) (#def fg) := tabInserta(n, validDst, !(#def fg))
                                                            else ()
                                                    val _ = if not (List.null validSrc) then (*(print("Inserte "); List.map (fn x => print(x ^ ", ")) validSrc; print(" en use\n");*) (#use fg) := tabInserta(n, validSrc, !(#use fg))
                                                            else ()
                                                    val _ = (#ismove fg) := tabInserta(n, false, !(#ismove fg))
                                                    val _ = case jmp of
                                                               NONE => if lastNode then ()
                                                                       else mk_edge (#control fg) {from=n, to=n2}
                                                             | SOME l => let 
                                                                            val labnode_l = map (getLabelNode labelList) l 
                                                                            val _ = map (fn nl => mk_edge (#control fg) {from=n, to=nl}) labnode_l
                                                                         in 
                                                                           ()
                                                                         end
                                                in 
                                                    ()
                                                end
                          | MOVE {dst = dst, src = src, ...} => let (*val _ = print("DST: " ^ dst ^ "\n");
                                                                    val _ = print("SRC: " ^ src ^ "\n");*)
                                                                    val _ = if admittedRegs dst then (*(print("Inserte " ^ dst ^ " en dst\n"); *)(#def fg) := tabInserta(n, [dst], !(#def fg))
                                                                            else ()
                                                                    val _ = if admittedRegs src then (* (print("Inserte " ^ src ^ " en use\n");*) (#use fg) := tabInserta(n, [src], !(#use fg))
                                                                            else ()
                                                                    val _ = (#ismove fg) := tabInserta(n, true, !(#ismove fg))
                                                                    val _ = if lastNode then ()
                                                                            else mk_edge (#control fg) {from=n, to=n2}
                                                                in 
                                                                    ()
                                                                end
                          | _ => let val _ = tabInserta(n, false, !(#ismove fg))
                                     val _ = if lastNode then ()
                                             else mk_edge (#control fg) {from=n, to=n2}
                                 in
                                    ()
                                 end
                        
             in 
                instrs2graph' xs (n + 1) (FGRAPH fg) labelList
             end
     
        val flow = instrs2graph' l 0 (FGRAPH fg) labelList
    in
        (*(printGraphFlow (FGRAPH flow);*)
         (FGRAPH flow, iTable)
    end
                                  
end

