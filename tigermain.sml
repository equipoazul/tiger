open tigerlex
open tigergrm
open tigerescap
open tigerseman
(*open tigerassem*)
open tigercodegen
open tigerflow
open tigerframe
open tigerassem
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
		
		
		
       fun replaceTforColors instrs colors =
        let
          (* Funcion que dado un T busca su color *)
          fun getColor t =
            let 
              val c = case tigertab.tabBusca(t, colors) of
                         SOME col => col
                       | NONE => t (*raise Fail "No deberia pasar " ^ t ^ "(replaceTforColors)"*)
            in
              c
            end

          fun replaceInstr (LABEL l) = LABEL l
            | replaceInstr (MOVE {assem=a, dst=d, src=s}) = MOVE {assem=a, dst=(getColor d), src=(getColor s)}
            | replaceInstr (OPER {assem=a, dst=d, src=s, jump=j}) = OPER {assem=a, dst=List.map getColor d, src=List.map getColor s, jump=j}
        in
          map replaceInstr instrs
        end
        
        fun printColorTable colors = (print "Tabla de colores:\n"; map (fn (x, y) => print (x ^ ": "^ y ^ "\n")) (tigertab.tabAList colors))

		fun name (tigerframe.PROC{body, frame}) = tigerframe.name(frame)
		  | name _ = raise Fail "error interno (name): no es PROC"
		val frags = tigertrans.getResult() (* lista de fragmentos *)
		val func_frags = let fun isFunc (tigerframe.PROC _) = true 
		                       | isFunc _ = false
		                 in
		                   List.filter isFunc frags
		                 end

		val str_frags = let fun isStr (tigerframe.STRING _) = true
		                      | isStr _ = false
		                    fun strip (tigerframe.STRING f) = f
		                      | strip _ = raise Fail "error interno (strip): no es STRING"
		                in
		                   List.map strip (List.filter isStr frags)
		                end
		                
     fun remRedundantMoves [] = []
       | remRedundantMoves ((i as MOVE {assem=a, dst=d, src=s})::is) = if d = s then remRedundantMoves is
                                                                 else (i::remRedundantMoves is)
       | remRedundantMoves (i::is) = (i::remRedundantMoves is)

	
		fun canonizar n = tigercanon.traceSchedule o (tigercanon.basicBlocks n) o tigercanon.linearize
		fun canon_frag (p as tigerframe.PROC {body, frame}) = (canonizar (name p) body, frame) 
		(* fun canon_frag (tigerframe.PROC {body, frame}) = (tigercanon.linearize body, frame) *)
		  | canon_frag _ = raise Fail "error interno (canon_frag): no es proc"
		val canon_frags = List.map canon_frag func_frags
                val instrlist = let
                          fun aplanar (x, frame) = List.map (fn y => (frame, y)) x

                          fun applyCodeGen stmList frame = List.map (fn x => tigercodegen.codegen frame x) stmList
                          val assemsBlocks = List.map (fn (x, y) => (applyCodeGen x y , y)) canon_frags
                          val plainAssemsBlocks = List.map (fn (x, y) => (List.concat x, y, true)) assemsBlocks
                          val precoloredCode = List.map tigercoloring.coloring plainAssemsBlocks
                          val coloredCode = List.map (fn (i, f, c) => (replaceTforColors i c, f)) precoloredCode
                          val procExitedCode = List.map (fn (x, y) => (tigerframe.procEntryExit3 (y, x), y)) coloredCode
                          
                          
                          val colprint = map remRedundantMoves (List.map (fn (x,y) => x) procExitedCode)
                          val stringSection = map tigerframe.string str_frags
                          val globlSection = map tigerframe.globl (List.map name func_frags)
	                        val codeSection = map (tigerassem.strAssem) (List.concat colprint)
	                        
	                        val allProgram = String.concat ([".data\n"] @ stringSection @ [".text\n\t.globl _tigermain\n"] @ codeSection)
	                        (*val _ = print "\n\nCodigo despues del coloreo:\n"
                          val _ = print allProgram*)
	                        (* Pasamos el assembler a un archivo y lo linkeamos con gcc *)
                          val fd = TextIO.openOut "asgard.s"
                          val _ = TextIO.output(fd, allProgram)
                          val _ = TextIO.closeOut fd
                          val _ = Process.system("gcc -m32 -c runtime.c")
                          val _ = Process.system("gcc -m32 -g runtime.o asgard.s -o outtiger")
                                                    
                        in
                           ()
                        end
        
	in
		print "yes!!\n"
	end	handle Fail s => print("Fail: "^s^"\n")

val _ = main(CommandLine.arguments())
