open tigerlex
open tigergrm
open tigerescap
open tigerseman
(*open tigerassem*)
open tigercodegen
open tigerflow
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "fin!")

fun println s = print(s^"\n")

fun concatWith c l = List.foldl (fn (x, xs) => x ^ c ^ xs) "" l

fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()
		val _ = transProg(expr)
		
		(* BORRAR *)
		fun name (tigerframe.PROC{body, frame}) = tigerframe.name(frame)
		  | name _ = raise Fail "error interno (name): no es PROC"
		(* BORRAR *)
		val frags = tigertrans.getResult() (* lista de fragmentos *)
		val _ = println("Fragmentos: " ^ Int.toString (List.length(frags)))
		val func_frags = let fun isFunc (tigerframe.PROC _) = true 
		                       | isFunc _ = false
		                 in
		                   List.filter isFunc frags
		                 end
		val _ = println("Fragmentos de funcion: " ^ Int.toString (List.length(func_frags)) ^ ": " ^ concatWith ", " (List.map name func_frags))                 
		val str_frags = let fun isStr (tigerframe.STRING _) = true
		                      | isStr _ = false
		                    fun strip (tigerframe.STRING f) = f
		                      | strip _ = raise Fail "error interno (strip): no es STRING"
		                in
		                   List.map strip (List.filter isStr frags)
		                end
		val _ = println("Fragmentos de string: " ^ Int.toString (List.length(str_frags)) ^ ": " ^ concatWith ", " (List.map (fn (l, s) => "(" ^ l ^ ", " ^ s ^ ")") str_frags))            
		val _ = println(tigertrans.Ir frags)    
		val canonizar = tigercanon.traceSchedule o tigercanon.basicBlocks o tigercanon.linearize
		fun canon_frag (tigerframe.PROC {body, frame}) = (canonizar body, frame) 
		(* fun canon_frag (tigerframe.PROC {body, frame}) = (tigercanon.linearize body, frame) *)
		  | canon_frag _ = raise Fail "error interno (canon_frag): no es proc"
		val canon_frags = List.map canon_frag func_frags
		(*val _ = tigerinterp.inter true canon_frags str_frags*)
        val instrlist = let
                          fun aplanar (x, frame) = List.map (fn y => (frame, y)) x
                          val stm_tpl = List.map aplanar canon_frags
                          val assems = List.concat (List.map (fn (x, y) => tigercodegen.codegen x y) (List.concat stm_tpl))
                          val _ = map tigerassem.printAssem assems
                          val graph = instrs2graph assems
                        in
                          (*List.map (fn (x, y) => tigercodegen.codegen x y) (List.concat stm_tpl) *)
                          (* map (fn ins => case ins of
                                        OPER {assem=x, ...} =>  print("OPER ->  " ^ x ^ "\n")
                                      | LABEL {assem=x, ...} =>  print("LABEL -> " ^ x ^ "\n")
                                      | MOVE {assem=x, ...} =>  print("MOVE ->  " ^ x ^ "\n")) (List.concat assems) *)
                          tigerflow.printGraphFlow (#1 graph)
                        end
        
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

(*fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
		val entrada =
			case l7 of
			[n] => ((open_in n)
					handle _ => raise Fail (n^" no existe!"))
			| [] => std_in
			| _ => raise Fail "opcio'n dsconocida!"
		val lexbuf = lexstream entrada
		val expr = prog Tok lexbuf handle _ => errParsing lexbuf
		val _ = findEscape(expr)
		val _ = if arbol then tigerpp.exprAst expr else ()
	in
		transProg(expr);
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n") *)

val _ = main(CommandLine.arguments())
