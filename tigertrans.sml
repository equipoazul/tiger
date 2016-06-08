structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero
    
type level = {parent:frame option , frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
    frame=newFrame{name="_tigermain"}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name} =
    {
    parent=SOME frame,
    frame=newFrame{name=name},
    level=level+1}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
(* cambiamos esto por la linea de abajo para que aguarde todas las variable en
* el stack
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame true *)
 fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b

fun formals{parent, frame, level} = tigerframe.formals frame

datatype exp =
    Ex of tigertree.exp
    | Nx of tigertree.stm
    | Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
    | seq [s] = s
    | seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
    | unEx (Nx s) = ESEQ(s, CONST 0)
    | unEx (Cx cf) =
    let
        val r = newtemp()
        val t = newlabel()
        val f = newlabel()
        val s = newlabel()
    in
        ESEQ(seq[cf (t, f),
                   LABEL f,
                   MOVE(TEMP r, CONST 0),
                   JUMP (NAME s, [s]),
                   LABEL t,
                   MOVE(TEMP r, CONST 1),
                   JUMP (NAME s, [s]),
                   LABEL s],
                   TEMP r)
    end

fun unNx (Ex e) = EXP e
    | unNx (Nx s) = s
    | unNx (Cx cf) =
    let
        val t = newlabel()
        val f = newlabel()
    in
        seq [cf(t,f),
            LABEL t,
            LABEL f]
    end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
    | unCx (Cx cf) = cf
    | unCx (Ex (CONST 0)) =
    (fn (t,f) => JUMP(NAME f, [f]))
    | unCx (Ex (CONST _)) =
    (fn (t,f) => JUMP(NAME t, [t]))
    | unCx (Ex e) =
    (fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
    let fun aux(Ex e) = tigerit.tree(EXP e)
        | aux(Nx s) = tigerit.tree(s)
        | aux _ = raise Fail "bueno, a completar!"
        fun aux2(PROC{body, frame}) = aux(Nx body)
        | aux2(STRING(l, "")) = l^":\n"
        | aux2(STRING("", s)) = "\t"^s^"\n"
        | aux2(STRING(l, s)) = l^":\t"^s^"\n"
        fun aux3 [] = ""
        | aux3(h::t) = (aux2 h)^(aux3 t)
    in  aux3 e end

(* While y for necesitan la u'ltima etiqueta para un break *)
local
    val salidas: label option tigerpila.Pila = (tigerpila.nuevaPila1 NONE)
in
    val pushSalida = tigerpila.pushPila salidas
    fun popSalida() = tigerpila.popPila salidas
    fun topSalida() =
        case tigerpila.topPila salidas of
        SOME l => l
        | NONE => raise Fail "break incorrecto!"            
end

val datosGlobs = ref ([]: frag list)


fun procEntryExit{level: level, body} =
    let val label = STRING(name(#frame level), "")
        val body' = PROC{frame= #frame level, body=unNx body}
    in  datosGlobs:=(!datosGlobs@[label, body']) end
fun getResult() = !datosGlobs

fun stringLen s =
    let fun aux[] = 0
        | aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
        | aux(_::t) = 1+aux(t)
    in  aux(explode s) end

    
fun stringExp(s: string) =
    let val l = newlabel()
        val str = "\""^s^"\""
        val _ = datosGlobs:=(!datosGlobs @ [STRING(l, str)])
    in  Ex(NAME l) end


fun preFunctionDec() =
    (pushSalida(NONE);
    actualLevel := !actualLevel+1)

fun functionDec(e, l, proc) =
    let val body =
                if proc then unNx e
                else MOVE(TEMP rv, unEx e)
        val body' = procEntryExit1(#frame l, body)
        val _ = procEntryExit{body=Nx body', level=l}
    in  
       Ex (TEMP rv)
    end


fun postFunctionDec() =
    (popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun simpleVar(acc, nivel) =
    case acc of
        InReg r => Ex (TEMP r)
        |InFrame k => 
            let 
                fun aux 0 = TEMP fp
                    |aux n = if n < 0 then raise Fail "(simpleVar) Error de memoria!\n"
                             else MEM (BINOP (PLUS, aux(n - 1), CONST fpPrevLev))
             in
                if (!actualLevel = nivel) then Ex (MEM (BINOP (PLUS, TEMP fp, CONST k))) 
                  else if (nivel < !actualLevel) then Ex (MEM (BINOP (PLUS, aux(!actualLevel - nivel), CONST k)))
                  else Ex (TEMP fp) 
            end

fun varDec(acc) = simpleVar(acc, getActualLev())

fun fieldVar(var, field) = 
let
    val a = unEx var
    val ra = newtemp()
    val ri = newtemp()
in
    Ex( ESEQ(seq[MOVE(TEMP ra, a),
        MOVE(TEMP ri, CONST field),
        EXP(externalCall("_checkNil", [TEMP ra]))],
        MEM(BINOP(PLUS, TEMP ra,
            BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

fun subscriptVar(arr, ind) =
let
    val a = unEx arr
    val i = unEx ind
    val ra = newtemp()
    val ri = newtemp()
in
    Ex( ESEQ(seq[MOVE(TEMP ra, a),
        MOVE(TEMP ri, i),
        EXP(externalCall("_checkIndexArray", [TEMP ra, TEMP ri]))],
        MEM(BINOP(PLUS, TEMP ra,
            BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

fun recordExp l =
let
    val li = List.map (fn (e, i) => unEx e) l
    val len = CONST (List.length l)
in
    Ex (externalCall("_allocRecord", len::li))
end

fun arrayExp{size, init} =
let
    val s = unEx size
    val i = unEx init
in
    Ex (externalCall("_allocArray", [s, i]))
end

fun callExp (name, external, isproc, lev:level, ls) = 
    let 
        fun memArray 0 = TEMP fp
           |memArray n = if n < 0 then raise Fail "Error de memoria"
                         else MEM (BINOP (PLUS, memArray (n-1), CONST fpPrevLev))
           val fplex = if (#level lev) = !actualLevel then (MEM (BINOP (PLUS, TEMP fp, CONST fpPrevLev)))
                           else if (#level lev) < !actualLevel then  memArray (!actualLevel - #level lev)
                           else TEMP fp
        fun preparaArgs [] (rt, re) = (rt, re)
           |preparaArgs (h::t) (rt, re) = 
                        case h of
                            Ex (CONST s) => preparaArgs t (CONST s::rt, re)
                           |Ex (NAME s) => preparaArgs t (NAME s::rt, re)
                           |Ex (TEMP s) => preparaArgs t (TEMP s::rt, re)
                           |_ => let val t' = newtemp()
                                 in
                                    preparaArgs t ([TEMP t']@rt, (MOVE (TEMP t', unEx h))::re)
                                 end
        val (la, la') = preparaArgs (rev ls) ([], [])
        val ta' = if external then la else fplex::la
    in 
        if isproc then Nx (seq(la'@[EXP (CALL (NAME name, ta'))]))
        else
            let val tmp = newtemp()
            in
                Ex (ESEQ (seq(la'@[EXP (CALL (NAME name, ta')),
                                   MOVE (TEMP tmp, TEMP rv)]), TEMP tmp))
            end
    end


fun breakExp() =
  let 
     val a = topSalida()
  in 
    Nx (JUMP (NAME a, [a]))
 end

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
    | seqExp (exps:exp list) =
        let
            fun unx [e] = []
                | unx (s::ss) = (unNx s)::(unx ss)
                | unx[] = []
        in
            case List.last exps of
                Nx s =>
                    let val unexps = map unNx exps
                    in Nx (seq unexps) end
                | Ex e => Ex (ESEQ(seq(unx exps), e))
                | cond => Ex (ESEQ(seq(unx exps), unEx cond))
        end


fun addAccFrame access (level:level) =  tigerframe.addAccFrame access (#frame level) (*((#frame) level) @ [access]*)

fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = let
                            val moves = List.map (fn {var=v, exp=e} => MOVE (unEx v, unEx e)) inits
                           in
                             Ex (ESEQ(seq moves,unEx body))
                           end
                           
fun preWhileForExp() = let
                          val l = newlabel()
                       in
                          pushSalida(SOME(l))
                       end

fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
    val cf = unCx test   (*JUMP( NAME t, [t]) *)
    val expb = unNx body
    val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
    Nx (seq[LABEL l1,
                cf(l2,l3),
              LABEL l2,
                 expb,
                 JUMP(NAME l1, [l1]),
              LABEL l3])
end


fun forExp {lo, hi, var, body} =
    let val var' = unEx var
        val (l1, l2, lsal) = (newlabel(), newlabel(), topSalida())
    in
        Nx (seq(case hi of
                Ex (CONST n) =>
                          if n = 999999 then (*haremos un while*)
                             [MOVE (var', unEx lo),
                              JUMP (NAME l2, [l2]),
                              LABEL l1, 
                                  unNx body,
                                  MOVE (var', BINOP (PLUS, var', CONST 1)),
                              LABEL l2,
                                CJUMP (GT, var', CONST n, lsal, l1),
                              LABEL lsal]
                        else 
                            [MOVE (var', unEx lo),
                             LABEL l2,
                                CJUMP (EQ, var', CONST n, lsal, l1),
                             LABEL l1,
                                unNx body,
                                MOVE (var', BINOP (PLUS, var', CONST 1)),
                                JUMP (NAME l2, [l2]),
                             LABEL lsal]
                |_  => (*high no es CONST*)
                        let 
                          val t = newtemp()
                        in
                          [MOVE (var', unEx lo),
                             MOVE (TEMP t, unEx hi),
                             CJUMP (GE, var', TEMP t, lsal, l2),
                             LABEL l2,
                                unNx body,
                                CJUMP (EQ, var', TEMP t, lsal, l1),
                             LABEL l1,
                                MOVE (var', BINOP (PLUS, var', CONST 1)),
                                JUMP (NAME l2, [l2]),
                             LABEL lsal]
                         end))
        end
                         

fun ifThenExp{test, then'} =
    let
        val cf = unCx test
        val (l1, l2) = (newlabel(), newlabel())
        val expThen = unNx then'
    in
        Nx (seq[cf(l1,l2),
                    LABEL l1,
                      expThen,
                    LABEL l2])
    end



fun ifThenElseExp {test,then',else'} =
    let
        val cf = unCx test
        val cfe = unEx test
        val (l1, l2, l3, r) = (newlabel(), newlabel(), newlabel(), newtemp())
        val expThen = unEx then'
        val expElse = unEx else'
    in
        Ex ( ESEQ(seq([cf(l1,l2),
                       LABEL l1,
                        MOVE (TEMP r, expThen),
                        JUMP (NAME l3, [l3]),
                       LABEL l2,
                        MOVE (TEMP r, expElse),
                       LABEL l3]), TEMP r))

    end
    
fun ifThenElseExpUnit {test,then',else'} =
    let
        val cf = unCx test
        val (l1, l2, l3) = (newlabel(), newlabel(), newlabel())
        val expThen = unNx then'
        val expElse = unNx else'
    in
        Nx (seq[cf(l1,l2),
                    LABEL l1,
                      expThen,
                      JUMP (NAME l3, [l3]),
                    LABEL l2,
                      expElse,
                    LABEL l3])
    end

fun assignExp{var, exp} =
let
    val v = unEx var
    val vl = unEx exp
in
    Nx (MOVE(v,vl))
end

fun binOpIntExp {left, oper, right} = 
    let
        val r = newtemp()
        val oper = case oper of
                     PlusOp => PLUS
                    |MinusOp => MINUS 
                    |TimesOp => MUL
                    |DivideOp => DIV
                    | _ => raise Fail "Error interno al interpretar operaciones binarias internas"  
        val lexp = unEx left
        val rexp = unEx right
    in
        Ex (BINOP (oper, lexp, rexp))
    end

fun binOpIntRelExp {left,oper,right} =
    let
        (*val r = newtemp()*)
        val oper = case oper of
                      EqOp => EQ
                     |NeqOp => NE
                     |LtOp => LT
                     |LeOp => LE
                     |GtOp => GT
                     |GeOp => GE
                     | _ => raise Fail "Error interno al interpretar operaciones binarias internas"  
        val lexp = unEx left
        val rexp = unEx right
        val (t, f) = (newlabel(), newlabel())
    in
      Cx (fn (t,f) => CJUMP(oper, lexp, rexp, t, f))
    end
    


fun binOpStrExp {left,oper,right} =
    let
        val oper = case oper of
                          EqOp => EQ
                         |NeqOp => NE
                         |LtOp => LT
                         |LeOp => LE
                         |GtOp => GT
                         |GeOp => GE
                         | _ => raise Fail "Error interno al interpretar operaciones binarias internas"  
        val lexp = unEx left
        val rexp = unEx right
        val (l, r, res) = (newtemp(), newtemp(), newtemp())

    in
        Cx (fn (t, f) => (seq[MOVE(TEMP l, lexp),
                                  MOVE(TEMP r, rexp),
                                  MOVE(TEMP res, externalCall("_stringcmp", [TEMP l, TEMP r])),
                                  CJUMP(oper, TEMP res, CONST 0, t, f)]))
    end

fun stmToExp s = EXP (unEx s)

end




