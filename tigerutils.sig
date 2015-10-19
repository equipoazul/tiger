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

(* val printTigerTip : Tipo -> string*)
end
