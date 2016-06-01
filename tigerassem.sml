structure tigerassem = struct

type reg = string
type temp = tigertemp.temp
type label = tigertemp.label

datatype instr =
	OPER of {
		assem: string,
		dst: temp list,
		src: temp list,
		jump: label list option}
	| LABEL of {assem: string, lab: tigertemp.label}
	| MOVE of {assem: string, dst: temp, src: temp}

fun printInstr2 (OPER {assem=a, dst=d, src=s, jump=_}) = 
      let
        val srcStr = foldr (fn (x, xs) => x ^ " " ^ xs) "" s
        val dstStr = foldr (fn (x, xs) => x ^ " " ^ xs) "" d
      in
        "OPER {assem = "^a^" dst = " ^ dstStr ^ ", src = " ^ srcStr ^ ", _ jump = _}\n"
      end
  | printInstr2 (LABEL {assem=a, lab=l}) = "LABEL {assem = "^a^" lab = "^l^"}\n"
  | printInstr2 (MOVE {assem=a, dst=d, src=s}) = "MOVE {assem = "^a^", dst ="^d^", src ="^s^"}\n"
  
fun printInstr (OPER {assem=a, dst=d, src=s, jump= j}) = 
      let
        val srcStr = foldr (fn (x, xs) => x ^ " " ^ xs) "" s
        val dstStr = foldr (fn (x, xs) => x ^ " " ^ xs) "" d
        val jmpStr = case j of 
                         SOME l => foldr (fn (x, xs) => x ^ " " ^ xs) "" l
                       | _ => ""
      in
        "[Dst(Def):" ^ dstStr ^ "Src(Use): " ^ srcStr ^ " Jmp: " ^ jmpStr ^ "]\t\t\t" ^ a 
      end
  | printInstr (LABEL {assem=a, lab=l}) = "[Dst(Def):  Src(Use): ]\t\t\t" ^ a
  | printInstr (MOVE {assem=a, dst=d, src=s}) = "[Dst(Def): " ^ d^ " Src(Use): "^s^"]\t\t\t" ^ a
  
fun src2List (MOVE r) = [#src r]
| src2List (OPER r) = #src r
| src2List _ = []

fun dst2List (MOVE r) = [#dst r]
| dst2List (OPER r) = #dst r
| dst2List _ = []
  
fun printInstrList n [] = ""
  | printInstrList n (x::xs) = (print (Int.toString(n) ^ "\t" ^ printInstr x); printInstrList (n + 1) (xs))
	
(*val compare: instr -> instr -> instr*)
fun instrCompare (OPER {assem=a, dst=d, src=s, jump=j}, OPER {assem=a', dst=d', src=s', jump=j'}) =
let 
    val assemEq = a = a'
    val dstEq = d = d'
    val srcEq = s = s'
    val jumpEq = j = j'
val eq = assemEq andalso dstEq andalso srcEq andalso jumpEq
in
if eq then EQUAL
else LESS
end
| instrCompare (LABEL {assem=a, lab=l}, LABEL {assem=a', lab=l'}) = 
let 
    val assemEq = a = a'
    val labEq = l = l'
    val eq = assemEq andalso labEq
in
if eq then EQUAL
else LESS
end
| instrCompare (MOVE {assem=a, dst=d, src=s}, MOVE {assem=a', dst=d', src=s'}) =
let 
    val assemEq = a = a'
    val dstEq = d = d'
    val srcEq = s = s'
val eq = assemEq andalso dstEq andalso srcEq
in
if eq then EQUAL
else LESS
end
| instrCompare (_ , _) = LESS


val format =
	let	fun speak(assem,dst,src,jump) =
			let	
			    val saylab = tigertab.name
			    fun saytemp t = t
				fun f(#"`":: #"s":: i::rest) = 
					(explode("%"^saytemp(List.nth(src,ord i - ord #"0")))
						@ f rest)
				| f(#"`":: #"d":: i:: rest) = 
					(explode("%"^saytemp(List.nth(dst,ord i - ord #"0")))
						@ f rest)
				| f(#"`":: #"j":: i:: rest) = 
					(explode(saylab(List.nth(jump,ord i - ord #"0")))
						@ f rest)
				| f(#"`":: #"`":: rest) = #"`" :: f rest
				| f(#"`":: _ :: rest) = raise Fail "bad Assem format"
				| f(c::rest) = c::(f rest)
				| f nil = nil
			in	implode(f(explode assem)) end
	in	fn OPER{assem,dst,src,jump=NONE} =>
			"\t"^speak(assem,
				List.map tigertemp.temp2string dst,
				List.map tigertemp.temp2string src,nil)
		| OPER{assem,dst,src,jump=SOME j} =>
			"\t"^speak(assem,
				List.map tigertemp.temp2string dst,
				List.map tigertemp.temp2string src,j)
        | LABEL{assem,...} => assem
        | MOVE{assem,dst,src} =>
	        "\t"^speak(assem,
		        [tigertemp.temp2string dst],
		        [tigertemp.temp2string src],nil)
end
    fun strAssem i = (format i) ^ "\n"

    fun printAssem i = (print o strAssem) i

end

