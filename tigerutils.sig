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
val unionList: ''a list -> ''a list -> ''a list

(* val printTigerTip : Tipo -> string*)
end
