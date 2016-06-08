structure tigertips =
struct

type unique = unit ref
datatype Tipo = TUnit
	| TNil
	| TInt
	| TString
	| TArray of Tipo * unique
	| TRecord of (string * Tipo * int) list * unique
	| TFunc of Tipo list * Tipo
	| TTipo of string * Tipo option ref


fun   printTigerTip (TUnit) = "TUnit"
	| printTigerTip (TNil) = "TNil"
	| printTigerTip (TInt) = "TInt"
	| printTigerTip (TString) = "TString"
	| printTigerTip (TArray (t, u)) = "TArray " ^ (printTigerTip t)
	| printTigerTip (TRecord (ls, u)) = let 
		                                  val recordvals = foldr (fn ((l, t, n), xs) => "(" ^ l ^ ":" ^ (printTigerTip t) ^ ":" ^ Int.toString(n) ^ ")" ^ xs) "" ls
		                                in
		                               	 "TRecord [" ^ recordvals ^ "]"
	                                    end
	| printTigerTip (TFunc (tl, t)) = let 
		                                 val argtypes = foldr (fn (x, xs) => " " ^ (printTigerTip x) ^ " " ^ xs) "" tl 
		                              in
		                              	  "TFunc [" ^ argtypes ^ "] : " ^ (printTigerTip t)
	                              	  end
	| printTigerTip (TTipo (t, u)) = "TTip " ^ t

end
