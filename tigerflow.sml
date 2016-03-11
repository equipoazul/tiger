structure tigerflow :> tigerflow =
struct

    open tigergraph
    open tigertab
    open tigerassem
    
    val instrTable = tabNueva()
    val tempTable = tabNueva()
    val boolTable = tabNueva()
    
    val fg =  {control = gr,
               def = tempTable, 
               use = tempTable,
               ismove = boolTable}

    fun   createNodes [] = ()
        | createNodes (x::xs) = let
                                  val n = newNode (#control fg)
                                  val _ = tabInserta(x, n, instrTable)
                              in
                                  createNodes xs
                              end  
    
    fun getNode i = case tabBusca(i, instrTable) of
                             SOME n => n
                           | _ => raise Fail "Error al buscar instrucciones en la tabla"
    fun getLabelNode l = case tabBusca(LABEL {lab=l, assem="LABEL " ^ l ^":"}, instrTable) of
                             SOME n => n
                           | _ => raise Fail "Error al buscar instrucciones en la tabla"
    
    fun instrs2graph' [] = (fg, nodes (#control fg))
       | instrs2graph' (x::y::xs) = let
                                      val n1 = getNode x
                                      val n2 = getNode y
                                      val _ = case x of
                                                OPER {assem = s,
                                                      dst = dst,
                                                      src = src,
                                                      jump = NONE} => let
                                                                          val _ = tabInserta(n1, dst, #def fg) 
                                                                          val _ = tabInserta(n1, src, #use fg)
                                                                          val _ = tabInserta(n1, false, #ismove fg)
                                                                          val _ = mk_edge {from=n1, to=n2}
                                                                      in 
                                                                          ()
                                                                      end
                                                | OPER {assem = s,
                                                        dst = dst,
                                                        src = src,
                                                        jump = SOME l } => let val _ = tabInserta(n1, dst, #def fg)
                                                                               val _ = tabInserta(n1, src, #use fg)
                                                                               val _ = tabInserta(n1, false, #ismove fg)
                                                                               val labnode_l = map getLabelNode l
                                                                               val _ = map (fn nl => mk_edge {from=n1, to=nl}) labnode_l
                                                                           in
                                                                               ()
                                                                          end                                                                 
                                                | MOVE {dst = dst, src = src, ...} => let val _ = tabInserta(n1, [dst], #def fg)
                                                                                          val _ = tabInserta(n1, [src], #use fg)
                                                                                          val _ = tabInserta(n1, true, #ismove fg)
                                                                                          val _ = mk_edge {from=n1, to=n2}
                                                                                      in 
                                                                                          ()
                                                                                      end
                                                | _ => let val _ = tabInserta(n1, false, #ismove fg)
                                                           val _ = mk_edge {from=n1, to=n2}
                                                       in
                                                          ()
                                                       end
                                              
                                   in 
                                      instrs2graph' (y::xs)
                                   end
                                   
    fun instrs2graph l = (createNodes l; instrs2graph' l)
                                  
end

