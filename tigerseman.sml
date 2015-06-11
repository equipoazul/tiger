structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertrans
open topsort

type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
	tabNueva(),
	[("int", TInt), ("string", TString)])

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost) 
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila 
fun topLevel() = tigerpila.topPila levelPila

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
	tabNueva(),
	[("print", Func{level=topLevel(), label="print",
		formals=[TString], result=TUnit, extern=true}),
	("flush", Func{level=topLevel(), label="flush",
		formals=[], result=TUnit, extern=true}),
	("getchar", Func{level=topLevel(), label="getstr",
		formals=[], result=TString, extern=true}),
	("ord", Func{level=topLevel(), label="ord",
		formals=[TString], result=TInt, extern=true}),
	("chr", Func{level=topLevel(), label="chr",
		formals=[TInt], result=TString, extern=true}),
	("size", Func{level=topLevel(), label="size",
		formals=[TString], result=TInt, extern=true}),
	("substring", Func{level=topLevel(), label="substring",
		formals=[TString, TInt, TInt], result=TString, extern=true}),
	("concat", Func{level=topLevel(), label="concat",
		formals=[TString, TString], result=TString, extern=true}),
	("not", Func{level=topLevel(), label="not",
		formals=[TInt], result=TInt, extern=true}),
	("exit", Func{level=topLevel(), label="exit",
		formals=[TInt], result=TUnit, extern=true})
	])

fun tipoReal (TTipo (s, ref (SOME (t)))) = tipoReal t
  | tipoReal t = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true 
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2 )) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TTipo (_, r)) b =
		let
			val a = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (1)"
		in
			tiposIguales a b
		end
  | tiposIguales a (TTipo (_, r)) =
		let
			val b = case !r of
				SOME t => t
				| NONE => raise Fail "No debería pasar! (2)"
		in
			tiposIguales a b
		end
  | tiposIguales a b = (a=b)

fun transExp(venv, tenv) =
  let 
    fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
    fun trexp(VarExp v) = trvar(v)
    | trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
    | trexp(NilExp _)= {exp=nilExp(), ty=TNil}
    | trexp(IntExp(i, _)) = {exp=intExp i, ty=TInt}
    | trexp(StringExp(s, _)) = {exp=stringExp(s), ty=TString}
    | trexp(CallExp({func, args}, nl)) =
      let
        val formalsArgs = map (fn e => #ty (trexp e)) args
        fun  zipEq [] (y::ys) pos = error("Sobran argumentos.", pos)
            | zipEq (x::xs) [] pos =  error("Faltan argumentos.", pos)
            | zipEq [] [] pos = []
            | zipEq (x::xs) (y::ys) pos = (x, y) :: zipEq xs ys pos
        val envEntry = case tabBusca(func, venv) of
                          SOME (Func vals) => vals
                        | _ => error (func^" no es una función", nl)
        val b = List.foldr (fn (x, rest) => (tiposIguales (#1 x) (#2 x)) andalso rest) true (zipEq (#formals envEntry) formalsArgs nl)
      in  
        if not b then error("Error en los argumentos de la funcion (tipos)", nl) else {exp=nilExp(), ty=(#result envEntry)} 
      end
    | trexp(OpExp({left, oper=EqOp, right}, nl)) =
      let
        val {exp=expl, ty=tyl} = trexp left
        val {exp=expr, ty=tyr} = trexp right
      in
        if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit 
        then {exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=EqOp,right=expr} 
                                              else binOpIntRelExp {left=expl,oper=EqOp,right=expr}, ty=TInt}
        else error("Tipos no comparables", nl)
      end
    | trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
      let
        val {exp=expl, ty=tyl} = trexp left
        val {exp=expr, ty=tyr} = trexp right
      in
        if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit 
        then {exp=if tiposIguales tyl TString then binOpStrExp {left=expl,oper=NeqOp,right=expr} else binOpIntRelExp {left=expl,oper=NeqOp,right=expr}, ty=TInt}
        else error("Tipos no comparables", nl)
      end
    | trexp(OpExp({left, oper, right}, nl)) = 
      let
        val {exp=expl, ty=tyl} = trexp left
        val {exp=expr, ty=tyr} = trexp right
      in
        if tiposIguales tyl tyr then
          case oper of
            PlusOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
            | MinusOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
            | TimesOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
            | DivideOp => if tipoReal tyl=TInt then {exp=binOpIntExp {left=expl, oper=oper, right=expr},ty=TInt} else error("Error de tipos", nl)
            | LtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
            | LeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
            | GtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
            | GeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=if tipoReal tyl=TInt then binOpIntRelExp {left=expl,oper=oper,right=expr} else binOpStrExp {left=expl,oper=oper,right=expr},ty=TInt} else error("Error de tipos", nl)
            | _ => raise Fail "No debería pasar! (3)"
        else error("Error de tipos, operando tipos distintos.", nl)
      end
    | trexp(RecordExp({fields, typ}, nl)) =
      let
        (* Traducir cada expresión de fields *)
        val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

        (* Buscar el tipo *)
        val (tyr, cs) = case tabBusca(typ, tenv) of
                            SOME t => (case tipoReal t of
                                            TRecord (cs, u) => (TRecord (cs, u), cs)
                                            | _ => error(typ^" no es de tipo record", nl))
                            | NONE => error("Tipo inexistente ("^typ^")", nl)
        (* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde *)
        fun verificar _ [] [] = []
          | verificar _ (c::cs) [] = error("Faltan campos", nl)
          | verificar _ [] (c::cs) = error("Sobran campos", nl)
          | verificar n ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
            if s<>sy then error("Error de campo", nl)
            else if tiposIguales ty t then (exp, n)::(verificar (n+1) cs ds)
               else error("Error de tipo del campo "^s, nl)
        val lf = verificar 0 cs tfields
      in
        {exp=recordExp lf, ty=tyr}
      end
    | trexp(SeqExp(s, nl)) =
      let  
        val lexti = map trexp s
        val exprs = map (fn{exp, ty} => exp) lexti
        val {exp, ty=tipo} = hd(rev lexti)
      in  { exp=seqExp (exprs), ty=tipo } end
      
    | trexp(AssignExp({var=SimpleVar s, exp}, nl)) = 
    
    let
      val {exp=_, ty=tyvar} = case tabBusca(s, venv) of
                                SOME (VIntro _) => (error("Variable de solo lectura.", nl))
                                |_ => (trvar(SimpleVar s, nl))
                          
      val {exp=_, ty=tyexp} = trexp exp
    in 
      if tiposIguales tyexp tyvar then {exp=nilExp(), ty=TUnit}
      else error("Error de tipos en asignacion", nl)
    end   
    | trexp(AssignExp({var, exp}, nl)) =
    let
      val {exp=_, ty=tyexp} = trexp exp
      val {exp=_, ty=tyvar} = trvar(var, nl)
    in 
      if tiposIguales tyexp tyvar then 
          case var of
            FieldVar v,s => {exp=fieldVar (v, )}
            SubscriptVar v,e =>

            {exp=nilExp(), ty=TUnit}
      else error("Error de tipos en asignacion", nl)
    end
      
    | trexp(IfExp({test, then', else'=SOME else'}, nl)) =
      let val {exp=testexp, ty=tytest} = trexp test
          val {exp=thenexp, ty=tythen} = trexp then'
          val {exp=elseexp, ty=tyelse} = trexp else'
      in
        case tipoReal tytest of
          TInt => if tiposIguales tythen tyelse then {exp=if tipoReal tythen=TUnit then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp} else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}, ty=tythen}
                  else error("Tipos distintos then-else.", nl)
          | _ => error("La condición del if debe ser entera.", nl)
      end
    | trexp(IfExp({test, then', else'=NONE}, nl)) =
      let val {exp=exptest,ty=tytest} = trexp test
          val {exp=expthen,ty=tythen} = trexp then'
      in
        case tipoReal tytest of 
          TInt => if tythen=TUnit then {exp=ifThenExp{test=exptest, then'=expthen}, ty=TUnit}
                  else error("El then debe retornar unit.", nl)
         | _ => error("La condición del if debe ser entera.", nl)
      end
    | trexp(WhileExp({test, body}, nl)) =
      let
        val ttest = trexp test
        val tbody = trexp body
      in
        if tipoReal (#ty ttest) = TInt andalso #ty tbody = TUnit then {exp=whileExp {test=(#exp ttest), body=(#exp tbody), lev=topLevel()}, ty=TUnit}
        else if tipoReal (#ty ttest) <> TInt then error("Error de tipo en la condición", nl)
        else error("El cuerpo de un while no puede devolver un valor", nl)
      end
    | trexp(ForExp({var, escape, lo, hi, body}, nl)) = 
      let  
        val {exp=explo, ty=tylo} = trexp lo
        val {exp=exphi, ty=tyhi} = trexp hi
        val level = getActualLev()
        val acc = allocLocal (topLevel()) (!escape) (*TODO El toplevel es para que tipe por ahora *)
        val _ = preWhileForExp()
        (* tabInserta(var, VIntro {parent=NONE, frame, level}, venv)*)
        val venv' = tabRInserta(var, VIntro {access=acc, level=level}, fromTab venv)
        val {exp=expbody, ty=tybody} = transExp (venv', tenv) body
        val expvar = simpleVar (acc, 0)
        val expfor = forExp {lo=explo, hi=exphi, var=expvar, body=expbody}
        val _ = postWhileForExp()
      in 
        case tipoReal tylo of
          TInt => (case tipoReal tyhi of
                    TInt => if tybody = TUnit then {exp=expfor, ty=tybody}
                            else error("El cuerpo de un for no puede devolver un valor", nl)
                    |_ => error("La expresión 'hi' no es entera.", nl))
          |_ => error("La expresión 'lo' no es entera.", nl)
      end 
    | trexp(LetExp({decs, body}, _)) =
      let
        val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
        val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
      in 
        {exp=expbody, ty=tybody}
      end 
    | trexp(BreakExp nl) =
      {exp=nilExp(), ty=TUnit} 
      
    | trexp(ArrayExp({typ, size, init}, nl)) =
      let
        val tyinit = trexp init
        val t = case tabBusca(typ, tenv) of (* REVISAR *)
                   SOME (TArray t2) => if tiposIguales (#1(t2)) (#ty tyinit) then {exp=nilExp(), ty=t2} 
                                     else error("Tipo de init distinto del tipo de array", nl)
                  | _ => error("Tipo de arreglo inexistente", nl)
        val tysize = trexp size
      in 
        if (#ty tysize) <> TInt then error("Tamaño de array inválido",nl) 
        else {exp=nilExp(), ty=TArray (#ty t)}
      end
      
    and trvar(SimpleVar s, nl) =
      (case tabBusca(s, venv) of 
         SOME (Var {access=acc, ty=t, level=lv}) => {exp=simpleVar(acc, lv), ty=t}
        |SOME (VIntro {access=acc, level=lv}) => {exp=simpleVar(acc, lv), ty=TInt} 
        | _ => error("Variable inexistente", nl))
    (*fieldvar-> v.s*) 
    | trvar(FieldVar(v, s), nl) =
      let
        val {exp=expvar, ty=typ} = trvar(v, nl)
        val (l, u) = (case (tipoReal typ) of
                        TRecord l' => l'                  
                        | _ => error(s^" No es un record", nl))
        in
          case List.find (fn x =>(#1)x = s) l of
              SOME (str, typfv, index) => {exp=unitExp()), ty=typfv} (*TODO, hay que usar fieldvar pero no se que va en los parametros*)
            | _ => error("Campo de record \""^s^"\" inexistente", nl)
        end

    | trvar(SubscriptVar(v, e), nl) =
      let
        val {exp=expvar, ty=typ} = trvar(v, nl)
      in
        (case typ of
              TArray (t, u) =>  (case (trexp e) of
                                      {exp=expe, ty = TInt} => {exp=subscriptVar(expe, expvar), ty=t}
                                      | _ => error("La expresión utilizada como índice no es entero.", nl))
              | _ => error("No es un arreglo", nl))
      end

    and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
          let 
            val _ = case init of
                      NilExp _ => error("No se puede asignar 'nil' en una declaración.", pos) 
                      |BreakExp _ => error("No se puede asignar 'nil' en una declaración.", pos) 
                      | _ => ()
            val {exp=_, ty=tyinit} = transExp (venv, tenv) init
            val acc = allocLocal (topLevel()) (!escape)
            val level = getActualLev()
            val venv' = tabInserta(name, Var{access=acc, level=level, ty=tyinit}, venv)
          in 
            (venv', tenv, [])
          end   
    
    | trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
        let
          val {exp=expe, ty=tyinit} = transExp (venv, tenv) init
          val acc = allocLocal (topLevel()) (!escape)
          val level = getActualLev()
          val venv' = (case tabBusca(s, tenv) of
                          SOME t => if tiposIguales t tyinit then tabInserta(name, Var{access=acc, level=level, ty=t}, venv)
                                    else error("el tipo de var es distinto a la expresion", pos)
                          |_ => error("tipo desconocido", pos))

        in 
           (venv', tenv, [])  
        end
		
    | trdec (venv,tenv) (FunctionDec fs) =
      let
        (*Chequeamos que no haya dos funciones con el mismo nombre *)
        val nlist = map (fn (x, pos) => (#name x, pos)) fs
          
        fun checkNames [] = true
            | checkNames ((x, pos)::xs) = if List.exists (fn y => x=(#1 y)) xs then error("Nombre '"^x^"' duplicado", pos)
                                          else checkNames xs 
        val _ = checkNames nlist                          

        fun transparams pos x = (case (#typ x) of
                                         NameTy s => (case tabBusca(s, tenv) of
                                                           SOME t => {name = (#name x), typ = t}
                                                          | _ => error("No se permiten argumentos de ese tipo.", pos))
                                         | _ => error("Error de tipo en el campo.", pos))

        val name' = tigertemp.newlabel()
        (* Obtiene la lista de argumentos de la funcion *)
        fun getFormals pos params = map (transparams pos) params 
        
        (* Obtiene el tipo de retorno de la funcion *)
        fun getResult pos x = (case x of
                                  SOME t => (case tabBusca(t, tenv) of
                                                SOME t' => t'
                                                |_ => error("Error en el tipo de retorno"^t^".", pos))
                                  |_ => TUnit)
        val venv' = foldr (fn (x, venvv) => tabInserta(#name (#1 x), Func{level=topLevel(), label=name', formals=map #typ (getFormals (#2 x) (#params (#1 x))), result=getResult (#2 x) (#result (#1 x)), extern=false}, venvv))  venv fs      
        fun procBody (r, pos) = 
            let
                val formals = getFormals pos (#params r) 
                  
                (* Aca hay que pasarle el venv' y no el venv por si el body hace una llamada recursiva, si sucede esto
                   el body deberia encontrar la funcion que llama (que es ella misma) *)
                val acc = allocLocal (topLevel()) false
                val level = getActualLev()
                val b_venv = foldr (fn (x, xs) => tabRInserta(#name x, Var {access=acc, level=level, ty=(#typ x)}, xs)) venv' formals 
                val {exp=bodyv, ty=bodyty} = transExp (b_venv, tenv) (#body r)
                val tipodeclarado = getResult 1 (#result r)
                val _ = if tiposIguales tipodeclarado TUnit then
                            if not (tiposIguales bodyty tipodeclarado) then 
                                error("No se puede retornar un valor en un procedure", 666)
                            else ()
                        else
                            if not (tiposIguales bodyty tipodeclarado) then 
                                error("El tipo declarado y el de retorno no coinciden", 666)
                            else ()
            in
                {exp=bodyv, ty=bodyty}
            end  
                     
        val _ = map procBody fs
        
      in 
        (venv', tenv, [])
      end 
    | trdec (venv,tenv) (TypeDec ts) =
        let
            
            fun buscaArrRecords lt = 
                let 
                    fun buscaRecs [] recs = recs
                        |buscaRecs ((r as {name, ty=RecordTy _}) :: t) recs = buscaRecs t (r :: recs)
                        |buscaRecs ((r as {name, ty=ArrayTy _}) :: t) recs = buscaRecs t (r :: recs)
                        |buscaRecs (_ :: t) recs = buscaRecs t recs
                in
                    buscaRecs lt [] end
                    
            fun genPares lt = 
                let  
                    fun genP [] res = res
                       |genP ({name, ty=NameTy s} :: t) res = genP t ((s, name) :: res)
                       |genP ({name, ty=ArrayTy s} :: t) res = genP t ((s, name) :: res)
                       |genP ({name, ty=RecordTy lf} :: t) res = genP t res
                in 
                    genP lt []                    
                       
                end
                

            fun procRecords batch recs env =
                let
                    fun buscaEnv env' t = (case tabBusca(t, env) of
                                            SOME (x as (TRecord _)) => TTipo (t, ref (SOME x))
                                            |SOME t' => t'
                                            |_ => (case List.find (fn {name, ...} => name = t) recs of
                                                        SOME {name, ...} => TTipo (name, ref NONE)
                                                        |_ => error(t^" no existe", 666)))
                    fun precs [] env' = env'
                        |precs ({name, ty=RecordTy lf} :: t) env' = 
                                let 
                                    val lf' = List.foldl (fn ({name, typ=NameTy t, ...}, l) => 
                                                                (name, buscaEnv env' t) :: l
                                                          |({name, typ=ArrayTy t, ...}, l) =>
                                                                (name, TArray (buscaEnv env' t, ref ())) :: l
                                                          |(_, l) => l) [] lf
                                    val (_, lf'') = List.foldl (fn ((x,y), (n,l)) => (n+1, (x, y, n) :: l)) (0, []) lf'
                                    val env'' = tabInserta(name, TRecord (lf'', ref ()), env') 
                                in
                                    precs t env''
                                end
                        |precs ({name, ty=ArrayTy t} :: tu) env' = precs tu (tabInserta (name, TArray (buscaEnv env' t, ref ()), env'))
                        |precs (_ :: tu) env' = precs tu env'
                in 
                    precs batch (fromTab env)
                end

            (*procesa ordered batch recs env*)
            fun procesa [] pares recs env = env
               |procesa (sorted as (h :: t)) pares recs env = 
                    let
                      fun filt h {name, ty=NameTy t} = h = t
                          |filt _ _ = false
                      val (ps, ps') = List.partition (filt h) (List.filter (fn x => (#name x) <> h) pares)
                      (* si List.find encuentra un elemento, quiere decir que h es un record o un 
                         array con lo cual lo metemos en el entorno luego con procesaRec*)

                      val hinrecs = List.find (fn {name,ty} => name = h) recs
                      val ttopt = (case hinrecs of 
									                     SOME _ => NONE
									                    |NONE => (case tabBusca (h, env) of
										                                 SOME t => SOME t
										                                |_ => error(h^" no existe.", 0)))

                      (* (1) Si encuentra un record o un array en este punto es probable que un NameTy dependa de uno de 
                         ellos, por lo tanto lo metemos en el enviroment ahora para que no haya errores de dependencia
                         que no son ciertos, esto lo tratamos en el "|_ => " de val env' *) 
										  
                      val env' = (case hinrecs of 
                                        SOME t => procRecords [t] [] env
                                       | _ => env)

                      fun meterEntorno env ts tt = List.foldr (fn ({name, ty=NameTy ty}, envr) => tabInserta(name, tt, envr)
                                                                |_ => error("error interno.", 0)) env ps
                      val env'' = (case ttopt of 
                                      SOME tt => meterEntorno env' ps tt

                                      |_ =>  case tabBusca (h, env') of
										                                 SOME tt => meterEntorno env' ps tt
										                                |_ => env' ) 
                    
                    in 
                        procesa t ps' recs env''
                    end
                

                
            fun fijaNONE [] env = env
                |fijaNONE ((name, TArray (TTipo (s, ref NONE), u)) :: t) env =
                    (case tabBusca(s, env) of 
                        SOME (r as (TRecord _)) => fijaNONE t (tabRInserta(name, TArray (TTipo(s, ref (SOME r)), u), env))
                        |_ => error("error interno.", 0))
                |fijaNONE ((name, TRecord (lf, u)) :: t) env =
                        let
                            fun busNONE ((s, TTipo (t, ref NONE), n), l) =
                                (case tabBusca(t, env) of
                                    SOME (tt as (TRecord _)) => (s, TTipo (t, ref (SOME tt)), n) :: l
                                    | SOME _ => error (s ^ " Error interno", 0)
                                    | _ => error (s^": Tipo inexistente", 0))
                               |busNONE (d, l) = d :: l
                            val lf' = List.foldr busNONE [] lf  
                        in                                
                            fijaNONE t (tabRInserta (name, TRecord (lf', u), env)) 
                        end
                |fijaNONE (_ :: t) env = fijaNONE t env
    
            fun fijaTipos batch env = 
                let val pares = genPares batch
                    val ordered = topsort pares 
                    val recs = buscaArrRecords batch
                    val env' = procesa ordered batch recs env
                    val env'' = procRecords batch recs env'
                    val env''' = fijaNONE ( tabAList env'') env''
                in
                    env'''
                end                    
                                  
           
            fun checkNames [] = true
            | checkNames ((x, pos)::xs) = if List.exists (fn y => x=(#1 y)) xs then error("Nombre '"^x^"' duplicado", pos)
                                          else checkNames xs  
            val decs = List.map #1 ts
            val _ = checkNames (List.map (fn (x, pos) => (#name x, pos)) ts)
            val tenv' = fijaTipos decs tenv               
        in
            (venv, tenv', []) 
        end
  in trexp end
fun transProg ex =
	let	val main =
				LetExp({decs=[FunctionDec[({name="_tigermain", params=[],
								result=SOME "int", body=ex}, 0)]],
						body=UnitExp 0}, 0)
		val _ = transExp(tab_vars, tab_tipos) main
	in	print "bien!\n" end
end
