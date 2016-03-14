structure tigerflow :> tigerflow =
struct

    open tigergraph
    open tigertab
    open tigerassem
  
    type instrTable = (tigergraph.node, tigerassem.instr) tigertab.Tabla
    type tempTable  = (tigergraph.node, tigertemp.temp list) tigertab.Tabla
    type boolTable  = (tigergraph.node, bool) tigertab.Tabla

    datatype flowgraph =
        FGRAPH of {control: tigergraph.graph,
                   def: tempTable ref,
                   use: tempTable ref,
                   ismove: boolTable ref}


    val iTable = ref (tabNueva())
    val defTable = ref (tabNueva())
    

    fun   createNodes [] fg labelList = labelList
        | createNodes (x::xs) (FGRAPH fg) labelList = let
                                                        val n = newNode (#control fg)
                                                        val _ = tabInserta(n, x, !iTable)
                                                       
                                                      in
                                                         case x of
                                                            LABEL {lab=l, ...} => createNodes xs (FGRAPH fg) ((l, n)::labelList)
                                                          | _ => createNodes xs (FGRAPH fg) labelList
                                                      end  
    
    fun getNode i = case tabBusca(i, !iTable) of
                             SOME n => (print "Holaaaa\n"; n)
                           | _ => raise Fail "Error al buscar instrucciones en la tabla 1"
                           
                          
    fun getLabelNode [] l' = raise Fail ("Error al buscar el label " ^ l' ^ " en la lista de labels.")
        | getLabelNode ((l, n)::xs) l' = if l = l' then n
                                         else getLabelNode xs l'
    

    fun printGraphFlow ((FGRAPH f): flowgraph) =
        let
            val g = #control(f)
            fun printTab t s = print(s ^ ": " ^ (String.concatWith "\n" (List.map (fn (n, ts) => tigergraph.nodename n ^ " -> " ^ (String.concatWith ", " ts) ^ " ") (tigertab.tabAList t))) ^ "\n")
        in
            print("FLOWGRAPH: \n");
            tigergraph.printGraph g;
            printTab (!(#use(f))) "Use";
            printTab (!(#def(f))) "Def"
        end
        
                                  
    fun instrs2graph l = 
      let 
        val bTable = ref (tabNueva())
        val useTable = ref (tabNueva())
        val (FGRAPH fg) = FGRAPH {control = gr,
                                  def = defTable, 
                                  use = useTable,
                                  ismove = bTable}
        val labelList = createNodes l (FGRAPH fg) []

                                                               
        fun instrs2graph' [] n fg labelList = fg
           | instrs2graph' (x::xs) n fg labelList = 
             let
                 
                val (n2, lastNode) = if xs <> [] then (n + 1, false)
                                     else (~1, true)
                val _ = case x of
                          OPER {assem = s,
                                dst = dst,
                                src = src,
                                jump = jmp} => let  val _ = (#def fg) := tabInserta(n, dst, !(#def fg)) 
                                                    val _ = (#use fg) := tabInserta(n, src, !(#use fg))
                                                    val _ = (#ismove fg) := tabInserta(n, false, !(#ismove fg))
                                                    val _ = case jmp of
                                                               NONE => if lastNode then ()
                                                                       else mk_edge {from=n, to=n2}
                                                             | SOME l => let 
                                                                            val labnode_l = map (getLabelNode labelList) l 
                                                                            val _ = map (fn nl => mk_edge {from=n, to=nl}) labnode_l
                                                                         in 
                                                                           ()
                                                                         end
                                                in 
                                                    ()
                                                end
                          | MOVE {dst = dst, src = src, ...} => let val _ = (#def fg) := tabInserta(n, [dst], !(#def fg))
                                                                    val _ = (#use fg) := tabInserta(n, [src], !(#use fg))
                                                                    val _ = (#ismove fg) := tabInserta(n, true, !(#ismove fg))
                                                                    val _ = if lastNode then ()
                                                                            else mk_edge {from=n, to=n2}
                                                                in 
                                                                    ()
                                                                end
                          | _ => let val _ = tabInserta(n, false, !(#ismove fg))
                                     val _ = if lastNode then ()
                                             else mk_edge {from=n, to=n2}
                                 in
                                    ()
                                 end
                        
             in 
                instrs2graph' xs (n + 1) fg labelList
             end
     
        val flow = instrs2graph' l 0 fg labelList
    in
        ((FGRAPH flow), nodes (#control flow))
    end
                                  
end

