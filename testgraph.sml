open BasicIO Nonstdio
open tigergraph
open tigerutils

(*val gr = newGraph ()*)


fun main(args) = let val n0 = newNode gr
                     val n1 = newNode gr
                     val n2 = newNode gr
                     val n3 = newNode gr
                     val _ = mk_edge {from = n0, to = n1}
                     val _ = mk_edge {from = n1, to = n2}
                     val _ = mk_edge {from = n1, to = n3}
                     val _ = mk_edge {from = n3, to = n0}
                     val _ = mk_edge {from = n0, to = n3}
(*                     val _ = rm_edge {from = n0, to = n1}*)

                     val _ = printNodes (nodes gr)
                     val _ = printEdges (edges gr)
                     val _ = printNodes (succ n3)
                     val _ = printNodes (pred n3)
                     val _ = printNodes (adj n3)
                    
                     
                 in 
                     ()
                 end
                 
val _ = main(CommandLine.arguments())
