(*
LA FUNCION FORMALS TIENE QUE DEVOLVER UNA LISSTA CON LOS ACCESOS DE LOS ARGUMENTOS DE LA FUNCION
SEGUN COMO SE USEN EN LA FUNCION INCLUIDO EL STATIC LINK (ARGUMENTO 0)
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree
open tigerassem

type level = int

(*val fp = "FP"               (* frame pointer *) 
val sp = "SP"				(* stack pointer *)
val rv = "RV"			    (* return value  *)
val ov = "OV"				(* overflow value (edx en el 386) *) *)
val fp = "ebp"				    
val sp = "esp"
val rv = "eax"

val ov = "edx"
val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = 8			(* offset (bytes) *)
val argsInicial = 3			(* words *)
val argsOffInicial = 0		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *)
val localsInicial = 0		(* words *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
(*
 * Terminology:
          o %eax, %ecx, %edx are "caller save" registers
          o %ebp, %ebx, %esi, %edi are "callee save" registers*)
val callersaves = ["ecx", "edx"]
val calleesaves = ["ebx", "edi", "esi"]
type register = string
datatype access = InFrame of int | InReg of tigertemp.label


fun  printAccess (InFrame k) = print ("InFrame " ^ Int.toString(k))
    |printAccess (InReg k) = print ("InReg " ^ k)

type frame = {
	name: string,
	formals: access list ref,
	locals: bool list,
	actualArg: int ref,
	actualLocal: int ref,
	actualReg: int ref
}
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	            | STRING of tigertemp.label * string
fun newFrame{name} = {
	name=name,
	formals=ref [InFrame(fpPrevLev)],
	(*formals=ref [],*)
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	actualReg=ref regInicial
}
fun addAccFrame access (frame:frame) = ((#formals frame) := !(#formals frame) @ [access] ; ())
fun name(f: frame) = #name f
fun string(l, "") = ""
  | string(l, s) = l^":\n\t.long " ^ Int.toString(String.size(s) - 2) ^ "\n\t.string "^tigertemp.makeString(s)^"\n\n"
fun globl(l) = "\t.globl\t "^ l ^"\n"
fun formals({formals=f, ...}: frame) = !f
	(*let	fun aux(n, []) = []
		| aux(n, h::t) = InFrame(n)::aux(n+argsGap, t)
	in aux(argsInicial, f) end *)
fun maxRegFrame(f: frame) = !(#actualReg f)
fun allocArg (f: frame) b = 
	case true of
	true =>
		let	val ret = (!(#actualArg f)*wSz+argsOffInicial)
			val _ = #actualArg f := !(#actualArg f)+1
		in	InFrame ret end
	| false => InReg(tigertemp.newtemp())
fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)*4 + localsGap)
		in	#actualLocal f:=(!(#actualLocal f)-1); ret end
	| false => InReg(tigertemp.newtemp())
fun exp(InFrame k) = MEM(BINOP(PLUS, TEMP(fp), CONST k))
| exp(InReg l) = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)
(*fun procEntryExit1 (frame,body) = body*)
fun seq [] = EXP (CONST 0)
	| seq [s] = s
	| seq (x::xs) = SEQ (x, seq xs)
fun procEntryExit1 (frame,body) = 
    let 
        val temps = List.map (fn _ => tigertemp.newtemp()) (calleesaves)
        val tempsAndRegs = ListPair.zip (temps, calleesaves)
        val moves = List.map (fn (t, r) => tigertree.MOVE(TEMP t, TEMP r)) tempsAndRegs
        val unMoves = List.map (fn (t, r) => tigertree.MOVE(TEMP r, TEMP t)) (List.rev tempsAndRegs) 
    in
        seq(moves @ [body] @ unMoves)
    end
(*fun procEntryExit1 (frame,body) = 
    let 
        fun getNewAlloc () = case (allocLocal frame true) of
                                      InFrame m' => m'
                                    | _ => raise Fail "En true esto no deberia pasar...."
        val temps = List.map (fn r => (r, getNewAlloc())) (calleesaves)
        val moves = List.map (fn (r, m) => tigertree.MOVE(MEM(BINOP(PLUS, TEMP "ebp", CONST m)), TEMP r)) temps
        val unMoves = List.map (fn (r, m) => tigertree.MOVE(TEMP r, MEM(BINOP(PLUS, TEMP "ebp", CONST m)))) (List.rev temps) 
    in
        seq(moves @ [body] @ unMoves)
    end*)


fun procEntryExit3 (frame:frame, instrs) = 
  let
    val label = [List.hd(instrs)]
    val prologo = [tigerassem.OPER {assem="enter $" ^ Int.toString((!(#actualLocal frame)) * (~4)) ^ ",$0x0\n", src=[], dst=[], jump=NONE}]
    val epilogo = [tigerassem.OPER {assem="leave\n", src=[], dst=[], jump=NONE},
                   tigerassem.OPER {assem="ret\n", src=[], dst=[], jump=NONE}]
  in
    label @ prologo @ (List.tl(instrs)) @ epilogo
  end


end
