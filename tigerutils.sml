structure tigerutils :> tigerutils = struct

open tigertree

(* ============================================================
         Funcion para la impresion de codigo intermedio 
   ============================================================ *)

fun printBrackets (s1, s2) = "(" ^ s1 ^ "," ^ s2 ^ ")"

fun   printExp (CONST n) = "CONST " ^ Int.toString(n)
	| printExp (TEMP l) = "TEMP " ^ l
	| printExp (NAME l) = "NAME " ^ l
	| printExp (BINOP (bop, e1, e2)) = "BINOP (" ^ (printBinOp bop) ^ ", " ^ (printExp e1) ^ "," ^ (printExp e2) ^ ")"
	| printExp (MEM e) = "MEM " ^ printExp e
	| printExp (CALL (e1, e2)) = let
								   val explist = foldr (fn (x, xs) => (printExp x) ^ ", " ^ xs) "" e2
	                             in
		                           "CALL (" ^ printBrackets(printExp e1, explist)
	                             end
	| printExp (ESEQ (s, e)) = "ESEQ (," ^  printBrackets(printStm s, printExp e)

and   printStm (MOVE (e1, e2)) = "MOVE " ^ printBrackets(printExp e1, printExp e2)
	| printStm (EXP e) = "EXP " ^ (printExp e)
	| printStm (JUMP (e, ls)) = let 
		                          val labelist = foldr (fn (x, xs) => x ^ ", " ^ xs) "" ls
		                        in 
		                          "JUMP " ^ printBrackets(printExp e, labelist)
		                        end 
    | printStm (CJUMP (relop, e1, e2, l1, l2)) = "CJUMP (" ^ (printRelOp relop) ^ (printExp e1) ^ ", " ^ (printExp e2) ^ ", " ^ l1 ^ ", " ^ l2 ^ ")"
	| printStm (SEQ (s1, s2)) = "SEQ " ^ printBrackets(printStm s1, printStm s2)  
	| printStm (LABEL l) = "LABEL " ^ l

and   printBinOp (PLUS) = "PLUS" 
	| printBinOp (MINUS) = "MINUS"
	| printBinOp (MUL) = "MUL" 
	| printBinOp (DIV) = "DIV"
	| printBinOp (AND) = "AND"
	| printBinOp (OR) = "OR"
	| printBinOp (LSHIFT) = "LSHIFT"
	| printBinOp (RSHIFT) = "RSHIFT"
	| printBinOp (ARSHIFT) = "ARSHIFT"
	| printBinOp (XOR) = "XOR"

and   printRelOp (EQ) = "EQ"
    | printRelOp (NE) = "NE"
    | printRelOp (LT) = "LT"
    | printRelOp (GT) = "GT"
    | printRelOp (LE) = "LE"
    | printRelOp (GE) = "GE"
    | printRelOp (ULT) = "ULT"
    | printRelOp (ULE) = "ULE"
    | printRelOp (UGT) = "UGT"
    | printRelOp (UGE) = "UGE"

end
