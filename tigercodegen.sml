(*
    Codegen para I386.

    Los casos ccte op ctte enteros se tratan aca'.
*)
structure tigercodegen :> tigercodegen =
struct

open tigertree
open tigerassem
open tigerframe 


structure T = tigertree

fun take 0 _ = []
| take _ [] = []
| take n (h::t) = h::take (n-1) t
fun drop 0 l = l
| drop _ [] = []
| drop n (h::t) = drop (n-1) t
fun zip (h::t) (m::n) = (h, m)::zip t n
| zip _ _ = []


fun printAsm asmlist = map (fn ins => case ins of
                                        OPER {assem=x, ...} =>  print("OPER ->  " ^ x ^ "\n")
                                      | LABEL {assem=x, ...} =>  print("LABEL -> " ^ x ^ "\n")
                                      | MOVE {assem=x, ...} =>  print("MOVE ->  " ^ x ^ "\n")) asmlist 
fun st n =
    if n=valOf Int.minInt then "-1073741824"
    else (if n<0 then "-" else "")^makestring(Int.abs n)

fun relOp relop =
    case relop of
    EQ =>       "je "   | NE =>     "jne "
    | LT =>     "jl "   | GT =>     "jg "
    | LE =>     "jle "  | GE =>     "jge "
    | ULT =>    "jb "   | ULE =>    "jbe "
    | UGT =>    "jae "  | UGE =>    "ja "
(* factor de escala para [base+index*escala] (viene como l2 por shift) *)
fun scaleFact n = n=0 orelse n=1 orelse n=2 orelse n=3
fun sF n = (* conversio'n a factor *)
    case n of
    0 => 1
    | 1 => 2
    | 2 => 4
    | 3 => 8
    | _ => raise Fail "conv. factor incorrecto! (error interno)"
fun codegen frame stm =
    let val ilist = ref (nil: instr list)
        fun emit x = ilist := x::(!ilist)
        fun result gen = let val t = tigertemp.newtemp() in gen t; t end
        fun munchStm s =
            case s of
            (SEQ(a, b)) => (munchStm a; munchStm b)
            | T.MOVE(TEMP t, CALL e) =>
              let
                  val assCall = munchExp (CALL e)
              in
                  (*emit(OPER{assem="movl `s0, `d0\n", src=[rv], dst=[t, rv], jump=NONE})*)
                  emit(MOVE{assem="movl `s0, `d0\n", src=rv, dst=t})
              end
            | T.MOVE(TEMP t1, MEM(BINOP(MINUS, TEMP t2, CONST i))) =>
                if t1=tigerframe.sp andalso t2=tigerframe.sp then
                    emit(OPER{assem="movl %esp, -"^Int.toString i^"(%esp)\n", src=[], dst=[], jump=NONE})
                else
                     emit(OPER{assem="movl -"^Int.toString i^"(`s0), `d0\n",
                        src=[t2], dst=[t1], jump=NONE})
                    (*emit(OPER {assem="movl `s0, `d0\n", src=[munchExp e],
                    dst=[t1], jump=NONE})*)
             (* TODO revisar este caso que debe ser como arriba *)
            | T.MOVE(TEMP t1, MEM(BINOP(PLUS, CONST i, TEMP t2))) =>
                if t2=tigerframe.fp then
                    emit(OPER{assem="movl "^ st(i) ^ "(%ebp), `d0 \n",
                        src=[], dst=[t1], jump=NONE})
                else
                    emit(OPER{assem="movl " ^ st(i) ^ "(`s0), `d0\n",
                        src=[t2], dst=[t1], jump=NONE})
            | T.MOVE(MEM(BINOP(PLUS, CONST i, e)), CONST j) =>
                emit(OPER{assem="movl " ^ st(i) ^ "(`s0) $" ^ st(j) ^ "\n",
                    src=[munchExp e], dst=[], jump=NONE})
            | T.MOVE(MEM(BINOP(PLUS, CONST i, TEMP t)), e2) =>
                if t=tigerframe.fp then
                    emit(OPER{assem="movl `s0, "^st(i)^"(%ebp)\n",
                        src=[munchExp e2], dst=[], jump=NONE})
                else
                    emit(OPER{assem="movl `s1, " ^ st(i) ^ "(`s0)\n",
                        src=[t, munchExp e2], dst=[], jump=NONE})
            | T.MOVE(MEM(BINOP(PLUS, CONST i, e1)), e2) =>
                emit(OPER{assem="movl `s1, " ^ st(i) ^ "(`s0)\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
            | T.MOVE(MEM e1, MEM e2) => (* 386 NO tiene M <- M *)
                let val t = tigertemp.newtemp()
                in
                    emit(OPER{assem="movl (`s0), `d0\n",
                        src=[munchExp e2], dst=[t], jump=NONE});
                    emit(OPER{assem="movl `s0, (`s1)\n",
                        src=[t,munchExp e1], dst=[], jump=NONE})
                end
            | T.MOVE(MEM e1, CONST i) =>
                emit(OPER{assem="movl $" ^ st(i) ^ ", (`s0)\n",
                    src=[munchExp e1], dst=[], jump=NONE})
                    
            | T.MOVE(MEM e1, NAME n) =>
                emit(OPER{assem="movl $" ^ n ^ ", (`s0)\n",
                    src=[munchExp e1], dst=[], jump=NONE})
            | T.MOVE(MEM e1, e2) =>
                emit(OPER{assem="movl `s1, (`s0)\n",
                    src=[munchExp e1, munchExp e2], dst=[], jump=NONE})
            | T.MOVE(TEMP i, CONST j) =>
                emit(OPER{assem="movl $"^st(j)^", `d0 \n",
                    src=[], dst=[i], jump=NONE})
            | T.MOVE(TEMP i, NAME l2) =>
                emit(OPER{assem="movl $"^l2^", `d0\n",
                    src=[], dst=[i], jump=NONE})
            | T.MOVE(TEMP i, e2) =>
                emit(MOVE{assem="movl `s0,`d0\n",
                    src=munchExp e2, dst=i})
            | T.MOVE(e1, e2) => 
                let val t=tigertemp.newtemp()
                in
                    emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e2, dst=t});
                    emit(MOVE{assem="movl `s0,`d0\n",
                        src=t, dst=munchExp e1})
                end
            | EXP(CALL(NAME n, args)) =>
                (saveCallerSaves();
                emit(OPER{assem="call "^n^"\n",
                    src=munchArgs args,
                    dst=calldefs, jump=NONE});                
                if length args - length argregs>0 then
                    emit(OPER{assem="addl $"^
                            st(wSz*(length(args)-length(argregs)))^", %"^sp^"\n",
                        src=[rv], dst=[rv]@tigerframe.callersaves, jump=NONE})
                else ();
                restoreCallerSaves())
            | EXP(CALL(e, args)) =>
                (saveCallerSaves();
                emit(OPER{assem="call `s0\n",
                    src=munchExp e::munchArgs args,
                    dst=calldefs, jump=NONE});  
                if length args-length argregs>0 then
                    emit(OPER{assem="addl $"^
                            st(wSz*(length(args)-length(argregs)))^", `d0\n",
                        src=[rv], dst=tigerframe.callersaves, jump=NONE})
                else ();
                restoreCallerSaves())
            | EXP e =>
                (* ACAAA gaga *)
                emit(MOVE{assem="movl `s0,`d0\n",
                    src=munchExp e, dst=tigertemp.newtemp()})
            | JUMP(NAME n, l) =>
                emit(OPER{assem="jmp "^n^"\n",
                        src=[], dst=[], jump=SOME l})
            | JUMP(e, l) =>
                let val e' = munchExp e
                in  emit(OPER{assem="jmp `s0\n",
                        src=[e'], dst=[], jump=SOME l}) end
            | CJUMP(relop, CONST c1, CONST c2, l1, l2) =>
                let 
                    val tmp = tigertemp.newtemp()
                    val _ = emit(OPER{assem="movl $"^st(c1)^", `d0\n",
                                      src=[], dst=[tmp], jump=NONE})                                      
                in
                    emit(OPER{assem="cmpl $" ^ st(c2) ^ ", `s0\n",
                              src=[tmp], dst=[tmp], jump=NONE});
                    emit(OPER{assem = relOp(relop) ^ l1 ^ "\n", 
                              src = [],
                              dst = [],
                              jump = SOME [l1, l2]})
                end


            | CJUMP(relop, e1, CONST c2, l1, l2) =>
                let (*val () = emit(OPER{assem="cmpl $" ^ st(c2) ^ ", `s0\n",
                        src=[munchExp e1], dst=[], jump=NONE})*)
                    val tmp = tigertemp.newtemp()
                    val _ = emit(OPER{assem="movl $"^st(c2)^", `d0\n",
                                      src=[], dst=[tmp], jump=NONE})
                    val () = emit(OPER{assem="cmpl `s1, `s0\n",
                        src=[munchExp e1, tmp], dst=[munchExp e1, tmp], jump=NONE})
                in  
                    emit(OPER{assem=relOp(relop)^l1^"\n", src=[],
                          dst=[], jump=SOME[l1, l2]})
                   
                end
            | CJUMP(relop, e1, e2, l1, l2) =>
                let (*val () = emit(OPER{assem="cmpl `s0,`s1\n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=NONE})*)
                    val () = emit(OPER{assem="cmpl `s1,`s0\n",
                        src=[munchExp e1, munchExp e2], dst=[munchExp e1, munchExp e2], jump=NONE})

                in  
                      emit(OPER{assem=relOp(relop)^l1^"\n", src=[],
                          dst=[], jump=SOME[l1, l2]})

                end
            | T.LABEL l => emit(LABEL{assem=l^":\n", lab=l})
        and saveCallerSaves() =
            let fun emitcdefs s =
                    emit(OPER{assem="pushl `s0\n", src=[s],
                                dst=[], jump=NONE})
            in  List.map emitcdefs tigerframe.callersaves end
        and restoreCallerSaves() =
            let fun emitcdefs s =
                    emit(OPER{assem="popl `d0\n", src=[],
                                dst=[s], jump=NONE})
            in  List.app emitcdefs (rev tigerframe.callersaves) end


        and munchArgs params =
            let fun munchArgsSt [] = []
                | munchArgsSt(h::t) = 
                    let val (instr, e) =
                            case h of
                            CONST i => (OPER{assem="pushl $"^st(i)^"\n",
                                        src=[], dst=[], jump=NONE}, "")
                            | NAME n => (OPER{assem="pushl $"^n^"\n",
                                        src=[], dst=[], jump=NONE}, "")
                            | TEMP n =>
                                    if n=tigerframe.fp then
                                        (OPER{assem="pushl %ebp\n",
                                        src=[], dst=[], jump=NONE}, "") (*before print "Elegimos bien0\n" *)
                                    else
                                        (OPER{assem="pushl `s0\n",
                                        src=[n], dst=[], jump=NONE}, "")
                            | MEM(TEMP n) =>
                                        if n=tigerframe.fp then
                                            (OPER{assem="pushl (%ebp)\n",
                                            src=[], dst=[], jump=NONE}, "") (* before print "Elegimos bien1\n" *)
                                        else
                                            (OPER{assem="pushl (`s0)\n",
                                            src=[n], dst=[], jump=NONE}, "")
                            | MEM(BINOP(PLUS, TEMP n, CONST c)) =>
                                        if n=tigerframe.fp then
                                            (OPER{assem="pushl "^st(c)^"(%ebp)\n",
                                            src=[], dst=[], jump=NONE}, "") (* before print "Elegimos bien1\n" *)
                                        else
                                            (OPER{assem="pushl " ^ st(c) ^ "(`s0)\n",
                                            src=[n], dst=[], jump=NONE}, "")
                            | _ =>  let val e = munchExp h
                                    in  (OPER{assem="pushl `s0\n", src=[e],
                                            dst=[], jump=NONE}, e) end
                    in  emit(instr);
                        if e<>"" then e::munchArgsSt t else munchArgsSt t
                    end
                    fun munchArgsRgs(e, r) =
                        emit(MOVE{assem="movl `s0, `d0\n", src=munchExp e, dst=r})
                    val ll = length tigerframe.argregs
                    val lr = take ll params
                    val ls = drop ll params
                in
                    List.app munchArgsRgs (zip lr tigerframe.argregs);
                    munchArgsSt (rev ls)
                end
        and munchExp e =
            case e of
            CONST i => st i
            | TEMP t => t
            | NAME l => l
            | MEM(BINOP(PLUS, NAME n, CONST i)) =>
                (result(fn r =>
                    emit(OPER{assem="movl ("^n^"+"^st(i)^"), `d0\n",
                        src=[], dst=[r], jump=NONE})))
            | MEM(BINOP(PLUS, e1, CONST i)) =>
                result(fn r =>
                    emit(OPER{assem="movl "^ st(i) ^ "(`s0), `d0\n",
                        src=[munchExp e1], dst=[r], jump=NONE}))
             (* TODO *)
            | MEM(BINOP(PLUS, TEMP t0, BINOP(LSHIFT, TEMP t1, CONST i))) =>
                if scaleFact i then
                    result(fn r =>
                        emit(OPER{assem="movl (`s0+"^st(sF i)^"*`s1), `d0\n",
                            src=[t0,t1], dst=[r], jump=NONE}))
                else
                    result(fn r =>
                           (*(emit(OPER{assem="movl `s0,`d0\n",
                            src=[t1], dst=[r], jump=NONE});*)
                        (emit(MOVE{assem="movl `s0,`d0\n",
                            src=t1, dst=r});
                        emit(OPER{assem="shll $"^st(i)^", `d0\n",
                            src=[], dst=[r], jump=NONE});
                        emit(OPER{assem="movl (`s0), `d0\n",
                            src=[r], dst=[r], jump=NONE})))
            | MEM(BINOP(MINUS, NAME n, CONST i)) =>
                result(fn r =>
                    emit(OPER{assem="movl ("^n^"-"^st(i)^"), `d0\n",
                        src=[], dst=[r], jump=NONE}))
            | MEM(BINOP(MINUS, e, CONST i)) =>
                result(fn r =>
                    emit(OPER{assem="movl (`s0-"^st(i)^"), `d0\n",
                        src=[munchExp e], dst=[r], jump=NONE}))
            | MEM e =>
                result(fn r =>
                    emit(OPER{assem="movl (`s0), `d0\n",
                        src=[munchExp e], dst=[r], jump=NONE}))
            | BINOP(PLUS, e, CONST i) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e, dst=r});
                    emit(OPER{assem="addl $"^st(i)^", `d0\n",
                        src=[r], dst=[r], jump=NONE})))
            | BINOP(PLUS, CONST i, e) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e, dst=r});
                    emit(OPER{assem="addl $"^st(i)^", `d0\n",
                        src=[r], dst=[r], jump=NONE})))
            | BINOP(PLUS, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="addl `s0,`d0\n",
                        src=[munchExp e2, r], dst=[r], jump=NONE})))
            (* los casos especiales de 0-exp, generados por el parser *)
            | BINOP(MINUS, CONST i, CONST j) =>
                result(fn r =>
                    (emit(OPER{assem="movl $"^st(i)^",`d0\n",
                        src=[], dst=[r], jump=NONE});
                    emit(OPER{assem="subl $"^st(j)^",`d0\n",
                        src=[r], dst=[r], jump=NONE})))
            | BINOP(MINUS, e1, CONST i) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="subl $"^st(i)^",`d0\n",
                        src=[r], dst=[r], jump=NONE})))
            (* ACAAA DA ERROR DEL NODO 0 EN EL MERGE (YA ESTA CORREGIDO PERO REVISAR) TODO *)
             | BINOP(MINUS, CONST i, e2) =>
                result(fn r =>
                    (emit(OPER{assem="movl $"^ st(i) ^ ", `d0\n",
                        src=[], dst=[r], jump=NONE});
                    emit(OPER{assem="subl `s0,`d0\n",
                        src=[munchExp e2, r], dst=[r], jump=NONE})))
            | BINOP(MINUS, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="subl `s0,`d0\n",
                        src=[munchExp e2, r], dst=[r], jump=NONE})))
            | BINOP(MUL, CONST j, CONST i) =>
              let 
                  val t = tigertemp.newtemp()
              in
                result(fn r =>
                    (emit(OPER{assem="movl $"^st(i)^",`d0\n",
                        src=[], dst=[t], jump=NONE});
                    emit(OPER{assem="imul $"^st(j)^", `s0, `d0\n",
                        src=[t, r], dst=[r], jump=NONE})))
              end
            | BINOP(MUL, e1, CONST i) =>
                result(fn r =>
                    emit(OPER{assem="imul $"^st(i)^", `s0, `d0\n",
                        src=[munchExp e1, r], dst=[r], jump=NONE}))
            | BINOP(MUL, CONST i, e2) =>
                result(fn r =>
                    emit(OPER{assem="imul $"^st(i)^", `s0, `d0\n",
                        src=[munchExp e2, r], dst=[r], jump=NONE}))
            | BINOP(MUL, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="imul `s0,`d0\n",
                        src=[munchExp e2, r], dst=[r], jump=NONE})))
            (* arghhh!! Intel y la @#! *)
            | BINOP(DIV, CONST i, CONST j) => (*yata*)
                let fun gen r =
                        (emit(OPER{assem="movl $"^st(i)^", `d0\n",
                            src=[], dst=[rv], jump=NONE});
                        emit(OPER{assem="movl $"^st(j)^", `d0\n",
                            src=[], dst=[r], jump=NONE});
                        emit(OPER{assem = "xorl `s0, `d0\n", src=[ov], dst=[ov], jump = NONE});
                        emit(OPER{assem="idivl `s0\n",
                            src=[r], dst=[rv,ov], jump=NONE}))
                in
                    result(fn r =>
                        emit(OPER{assem="movl $"^st(i div j)^", `d0\n",
                            src=[], dst=[r], jump=NONE})
                        handle Overflow => gen r
                        | Div => gen r)
                end
            | BINOP(DIV, e, CONST i) => (*yata*)
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n", src=munchExp e, dst=rv});
                    emit(OPER{assem="movl $"^st(i)^", `d0\n", src=[], dst=[r], jump=NONE});
                    emit(OPER{assem = "xorl `s0, `d0\n", src=[ov], dst=[ov], jump = NONE});
                    emit(OPER{assem="idivl `s0\n",
                        src=[r, rv], dst=[rv, ov], jump=NONE});
                    emit(MOVE{assem="movl `s0,`d0\n", src=rv, dst=r})))
            | BINOP(DIV, CONST i, e) =>
                result(fn r =>
                    (emit(OPER{assem="movl $"^st(i)^", `d0\n", src=[], dst=[rv], jump=NONE});
                    (emit(OPER{assem = "xorl `s0, `d0\n", src=[ov], dst=[ov], jump = NONE}));
                    emit(MOVE{assem="movl `s0,`d0\n", src=munchExp e, dst=r});
                    emit(OPER{assem="idivl `s0\n",
                        src=[r, rv], dst=[rv,ov], jump=NONE});
                    emit(MOVE{assem="movl `s0,`d0\n", src=rv, dst=r})))
            (*| BINOP(DIV, e1, e2) => (*yata*)
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n", src=munchExp e1, dst=rv});
                    (*emit(OPER{assem = "cwd\n", src=[], dst=[ov], jump = NONE});*)
                    emit(OPER{assem = "xorl `s0, `d0\n", src=[ov], dst=[ov], jump = NONE});
                    emit(OPER{assem="idivl `s0\n",
                        src=[munchExp e2, rv], dst=[rv,ov], jump=NONE});
                    emit(MOVE{assem="movl `s0,`d0\n", src=rv, dst=r})))*)
            | BINOP(DIV, e1, e2) => (*yata solo se rompe cuando dividimos 2 negativos y el denominador es -1*)
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n", src=munchExp e1, dst=rv});
                    emit(MOVE{assem="movl `s0,`d0\n", src=munchExp e2, dst=r});
                    emit(OPER{assem = "xorl `s0, `d0\n", src=[ov], dst=[ov], jump = NONE});
                    emit(OPER{assem="idivl `s0\n",
                        src=[r, rv], dst=[rv,ov], jump=NONE});
                    emit(MOVE{assem="movl `s0,`d0\n", src=rv, dst=r})))
            | BINOP(AND, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="andl `s1,`d0\n",
                        src=[munchExp e2,r], dst=[r], jump=NONE})))
            | BINOP(OR, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="orl `s1,`d0\n",
                        src=[munchExp e2,r], dst=[r], jump=NONE})))
            | BINOP(LSHIFT, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="shll `s0,`d0\n",
                        src=[munchExp e2,r], dst=[r], jump=NONE})))
            | BINOP(RSHIFT, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="shrl `s1,`d0\n",
                        src=[munchExp e2,r], dst=[r], jump=NONE})))
            | BINOP(ARSHIFT, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="sarl `s0,`d0\n",
                        src=[munchExp e2], dst=[r], jump=NONE})))
            | BINOP(XOR, e1, e2) =>
                result(fn r =>
                    (emit(MOVE{assem="movl `s0,`d0\n",
                        src=munchExp e1, dst=r});
                    emit(OPER{assem="xorl `s1,`d0\n",
                        src=[munchExp e2,r], dst=[r], jump=NONE})))
            | CALL(exp, explist) =>
                result(fn r =>
                    munchStm(T.EXP(CALL(exp, explist))))
            | ESEQ(stm, exp) => raise Fail "ESEQ incompleto!"
    in  munchStm stm; rev(!ilist) (* before print"salimos de codegen\n" *) end
end
