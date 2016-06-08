signature tigerutils = sig

type exp
type stm
type binop
type relop

val printExp : exp -> string
val printStm : stm -> string
val printBinOp : binop -> string
val printRelOp : relop -> string
val printBrackets : string*string -> string
val inList : ''a -> ''a list -> bool
val unionList: ((''a * ''a) -> order) -> ''a list -> ''a list -> ''a list
val listToSet: (('a * 'a) -> order) -> 'a list -> 'a Splayset.set
val tabToSet: (('a * 'a) -> order) -> ('b, 'a) tigertab.Tabla -> 'a Splayset.set
val tupleCompare: ((''a * ''a) -> order) -> (''a * ''a) * (''a * ''a) -> order
val singletonList : 'a list -> 'a list list
val setNthList: int -> ('a list) -> 'a -> ('a list)
(*val printColorArray: string array -> int -> tigerliveness.igraph -> unit*)

(* Stack *)
type 'a stack

val emptyStack : 'a stack 
val pop: 'a stack ref -> 'a 
val push: 'a -> 'a stack ref -> unit
val isEmptyStack: 'a stack ref -> bool
val stackToSet: (('a * 'a) -> order) -> 'a stack -> 'a Splayset.set 

end
