structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open topsort

fun printEnv [] = print (""^"\n")
    |printEnv (x::xs) = (print (x^"\n"); printEnv xs)


fun printEnv' ((name, TArray (t, u)) :: xs) = (print name; printEnv' [("asd",t)]; print "\n"; printEnv' xs)
    |printEnv' ((name, TRecord (lf, u)) :: xs) = (print name; printEnv (map #1 lf); printEnv' xs)
    |printEnv' ((name, TTipo _) :: xs) = (print name; print " TTipo"; printEnv' xs)
    |printEnv' _ = print "no me importa\n"

fun  zipEq [] (y::ys) = raise Fail "Error, distintos tamaños."
    | zipEq (x::xs) [] = raise Fail "Error, distintos tamaños."
    | zipEq [] [] = []
    | zipEq (x::xs) (y::ys) = (x, y) :: zipEq xs ys


type expty = {exp: unit, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
  tabNueva(),
  [("int", TInt), ("string", TString)])

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
  tabNueva(),
  [("print", Func{level=mainLevel, label="print",
    formals=[TString], result=TUnit, extern=true}),
  ("flush", Func{level=mainLevel, label="flush",
    formals=[], result=TUnit, extern=true}),
  ("getchar", Func{level=mainLevel, label="getstr",
    formals=[], result=TString, extern=true}),
  ("ord", Func{level=mainLevel, label="ord",
    formals=[TString], result=TInt, extern=true}),
  ("chr", Func{level=mainLevel, label="chr",
    formals=[TInt], result=TString, extern=true}),
  ("size", Func{level=mainLevel, label="size",
    formals=[TString], result=TInt, extern=true}),
  ("substring", Func{level=mainLevel, label="substring",
    formals=[TString, TInt, TInt], result=TString, extern=true}),
  ("concat", Func{level=mainLevel, label="concat",
    formals=[TString, TString], result=TString, extern=true}),
  ("not", Func{level=mainLevel, label="not",
    formals=[TInt], result=TInt, extern=true}),
  ("exit", Func{level=mainLevel, label="exit",
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
    | trexp(UnitExp _) = {exp=(), ty=TUnit}
    | trexp(NilExp _)= {exp=(), ty=TNil}
    | trexp(IntExp(i, _)) = {exp=(), ty=TInt}
    | trexp(StringExp(s, _)) = {exp=(), ty=TString}
    | trexp(CallExp({func, args}, nl)) =
      let
        val formalsArgs = map (fn e => #ty (trexp e)) args
        val envEntry = case tabBusca(func, venv) of
                          SOME (Func vals) => vals
                        | _ => error (func^" no es una función", nl)
        val b = List.foldr (fn (x, rest) => (tiposIguales (#1 x) (#2 x)) andalso rest) true (zipEq (#formals envEntry) formalsArgs)
      in  
        if not b then error("Error en los argumentos de la funcion (tipos)", nl) else {exp=(), ty=(#result envEntry)} 
      end
    | trexp(OpExp({left, oper=EqOp, right}, nl)) =
      let
        val {exp=_, ty=tyl} = trexp left
        val {exp=_, ty=tyr} = trexp right
      in
        if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
          else error("Tipos no comparables", nl)
      end
    | trexp(OpExp({left, oper=NeqOp, right}, nl)) = 
      let
        val {exp=_, ty=tyl} = trexp left
        val {exp=_, ty=tyr} = trexp right
      in
        if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit then {exp=(), ty=TInt}
          else error("Tipos no comparables", nl)
      end
    | trexp(OpExp({left, oper, right}, nl)) = 
      let
        val {exp=_, ty=tyl} = trexp left
        val {exp=_, ty=tyr} = trexp right
      in
        if tiposIguales tyl tyr then
          case oper of
            PlusOp => if tipoReal tyl=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | MinusOp => if tipoReal tyl=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | TimesOp => if tipoReal tyl=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | DivideOp => if tipoReal tyl=TInt then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | LtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | LeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | GtOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
            | GeOp => if tipoReal tyl=TInt orelse tipoReal tyl=TString then {exp=(),ty=TInt} else error("Error de tipos", nl)
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
        fun verificar [] [] = ()
          | verificar (c::cs) [] = error("Faltan campos", nl)
          | verificar [] (c::cs) = error("Sobran campos", nl)
          | verificar ((s,t,_)::cs) ((sy,{exp,ty})::ds) =
            if s<>sy then error("Error de campo", nl)
            else if tiposIguales ty t then verificar cs ds
               else error("Error de tipo del campo "^s, nl)
        val _ = verificar cs tfields
      in
        {exp=(), ty=tyr}
      end
    | trexp(SeqExp(s, nl)) =
      let  
        val lexti = map trexp s
        val exprs = map (fn{exp, ty} => exp) lexti
        val {exp, ty=tipo} = hd(rev lexti)
      in  { exp=(), ty=tipo } end
      
      
    (* <NOSOTROS> *)
    | trexp(AssignExp({var=SimpleVar s, exp}, nl)) = 
    
    let
      val {exp=_, ty=tyvar} = case tabBusca(s, venv) of
                                SOME VIntro => (error("Variable de solo lectura.", nl))
                                |_ => (trvar(SimpleVar s, nl))
      val {exp=_, ty=tyexp} = trexp exp
    in 
      if tiposIguales tyexp tyvar then {exp=(), ty=tyvar}
      else error("Error de tipos en asignacion", nl)
    end
    
    | trexp(AssignExp({var, exp}, nl)) =
    let
      val {exp=_, ty=tyexp} = trexp exp
      val {exp=_, ty=tyvar} = trvar(var, nl)
    in 
      if tiposIguales tyexp tyvar then {exp=(), ty=tyvar}
      else error("Error de tipos en asignacion", nl)
    end
    (* </NOSOTROS> *)
        
    | trexp(IfExp({test, then', else'=SOME else'}, nl)) =
      let val {exp=testexp, ty=tytest} = trexp test
          val {exp=thenexp, ty=tythen} = trexp then'
          val {exp=elseexp, ty=tyelse} = trexp else'
      in
        if tipoReal tytest=TInt andalso tiposIguales tythen tyelse then {exp=(), ty=tythen}
        else error("El tipo del if es incorrecto.", nl)
      end
    | trexp(IfExp({test, then', else'=NONE}, nl)) =
      let val {exp=exptest,ty=tytest} = trexp test
          val {exp=expthen,ty=tythen} = trexp then'
      in
        if tipoReal tytest=TInt andalso tythen=TUnit then {exp=(), ty=TUnit}
        else error("El tipo del if es incorrecto.", nl)
      end
    | trexp(WhileExp({test, body}, nl)) =
      let
        val ttest = trexp test
        val tbody = trexp body
      in
        if tipoReal (#ty ttest) = TInt andalso #ty tbody = TUnit then {exp=(), ty=TUnit}
        else if tipoReal (#ty ttest) <> TInt then error("Error de tipo en la condición", nl)
        else error("El cuerpo de un while no puede devolver un valor", nl)
      end
      
      (*NOSOTROS*)
      (*Revisar, Hay q verificar q el var es un SimpleVar???*)
      (*Es necesario meter el var en el env?? o eso es parte de la generacion de codigo intermedio??*)
    | trexp(ForExp({var, escape, lo, hi, body}, nl)) = 
      let 
             
        val tylo = trexp lo
        val tyhi = trexp hi
	      val venv' = tabInserta(var, VIntro, venv)
        val tybody = transExp (venv', tenv) body
      in 
        if tipoReal (#ty tylo) = TInt andalso (#ty tyhi) = TInt andalso (#ty tybody) = TUnit then (print "MORTADELA"; {exp=(), ty=TUnit})
        else if tipoReal (#ty tylo) <> TInt orelse #ty tyhi <> TInt then error("Error de tipo en la condición", nl)
        else error("El cuerpo de un for no puede devolver un valor", nl)
      end 
    
      (*/NOSOTROS*)
    
    
    
    
    
    | trexp(LetExp({decs, body}, _)) =
      let
        val (venv', tenv', _) = List.foldl (fn (d, (v, t, _)) => trdec(v, t) d) (venv, tenv, []) decs
        val {exp=expbody,ty=tybody}=transExp (venv', tenv') body
      in 
        {exp=(), ty=tybody}
      end 
    | trexp(BreakExp nl) =
      {exp=(), ty=TUnit} 
      
    | trexp(ArrayExp({typ, size, init}, nl)) =
      let
        val tyinit = trexp init
        val t = case tabBusca(typ, tenv) of (* REVISAR *)
                   SOME (TArray t2) => if tiposIguales (#1(t2)) (#ty tyinit) then {exp = () , ty = t2} 
                                     else error("Tipo de init distinto del tipo de array", nl)
                  | _ => error("Tipo de arreglo inexistente", nl)
        val tysize = trexp size
      in 
        if (#ty tysize) <> TInt then error("Tamaño de array inválido",nl) 
        else {exp = (), ty = TArray (#ty t)}
      end
      
      
    (* <NOSOTROS> *)
    and trvar(SimpleVar s, nl) =
        	(case tabBusca(s, venv) of 
		                   SOME (Var{ty = t}) => {exp = (), ty = t}
		                  | _ => error("Tipo de arreglo inexistente", nl))
		      

    | trvar(FieldVar(v, s), nl) =
      let
        val {exp = _, ty = typ} = trvar(v, nl)
        val (l, u) = (case typ of
                          TRecord l' => l'                  
                        | _ => error("No es un record", nl))
        in
          case List.find (fn x =>(#1)x = s) l of
              SOME (str, typfv, index) => {exp = (), ty = typfv}
            | _ => error("Campo de record \""^s^"\" inexistente", nl)
        end

    | trvar(SubscriptVar(v, e), nl) =
      let
        val {exp = _, ty = typ} = trvar(v, nl)
      in
        (case typ of
              TArray (t, u) =>  (case (trexp e) of
                                      {exp = _, ty = TInt} => {exp = (), ty = t}
                                      | _ => error("La expresión utilizada como índice no es entero.", nl))
              | _ => error("No es un arreglo", nl))
      end
    (* </NOSOTROS> *)

    and trdec (venv, tenv) (VarDec ({name,escape,typ=NONE,init},pos)) = 
          let 
            val {exp=_, ty=tyinit} = transExp (venv, tenv) init
            val venv' = tabInserta(name, Var{ty=tyinit}, venv)
          in 
            (venv', tenv, [])
          end   
    
    | trdec (venv,tenv) (VarDec ({name,escape,typ=SOME s,init},pos)) =
        let

          val {exp=_, ty=tyinit} = transExp (venv, tenv) init
          val venv' = (case tabBusca(s, tenv) of
                          SOME t => if tiposIguales t tyinit then tabInserta(name, Var{ty=t}, venv)
                                    else error("el tipo de var es distinto a la expresion", pos)
                          |_ => error("tipo desconocido", pos))

        in 
           (venv', tenv, [])  
        end




(*
    FunctionDec of ({name: symbol, params: field list,
		result: symbol option, body: exp} * pos) list
		
		field = {name: symbol, escape: bool ref, typ: ty}
		
		
		Func of {level: unit, label: tigertemp.label, formals: Tipo list, result: Tipo, extern: bool}
*)



		
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

        val venv' = foldr (fn (x, venvv) => tabInserta(#name (#1 x), Func{level=(), label=name', formals=map #typ (getFormals (#2 x) (#params (#1 x))), result=getResult (#2 x) (#result (#1 x)), extern=false}, venvv))  venv fs      

        fun procBody (r, pos) = 
            let
                val formals = getFormals pos (#params r) 
                (* Aca hay que pasarle el venv' y no el venv por si el body hace una llamada recursiva, si sucede esto
                   el body deberia encontrar la funcion que llama (que es ella misma) *)
                val b_venv = foldr (fn (x, xs) => tabInserta(#name x, Var {ty=(#typ x)}, xs)) venv' formals 
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
                (* transExp (b_venv, tenv) (#body r) *)
                {exp=bodyv, ty=bodyty}
            end  
                     
        val _ = map procBody fs
        
      in 
        (venv', tenv, [])
      end
     (*TypeDec of ({name: symbol, ty: ty} * pos) list
     list = {first:int, rest:list}
     field = {name: symbol, escape: bool ref, typ: ty}
     TRecord of (string * Tipo * int) list * unique *)   
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
                    (*val lrecs = buscaArrRecords lt*)
                    fun genP [] res = res
                       |genP ({name, ty=NameTy s} :: t) res = genP t ((s, name) :: res)
                       |genP ({name, ty=ArrayTy s} :: t) res = genP t ((s, name) :: res)
                       |genP ({name, ty=RecordTy lf} :: t) res = genP t res
                in 
                    genP lt []                    
                       
                end
                
            (*procesa ordered batch recs env*)
            fun procesa [] pares recs env = env
               |procesa (sorted as (h :: t)) pares recs env = 
                    let fun filt h {name, ty=NameTy t} = h = t
                            (*|filt h {name, ty=ArrayTy t} = h = t*)
                            |filt _ _ = false
                            
                            val (ps, ps') = List.partition (filt h) pares
                            (* si List.find encuentra un elemento, quiere decir que h es un record o un array con lo cual lo metemos en el entorno luego con procesaRec*)
                            val ttopt = (case List.find (fn {name,ty} => name = h) recs of 
												        SOME _ => NONE
												        |NONE => (case tabBusca (h, env) of
													        SOME t => SOME t
													        |_ => error(h^"no existe", 666)))
													  
                            val env' = (case ttopt of 
                                     SOME tt => List.foldr (fn ({name, ty=NameTy ty}, env') => tabInserta(name, tt, env)
                                                                  |_ => error("error interno1.", 666)) env ps
                                            |_ => env)
                    in 
                        procesa t ps' recs env'
                    end
                
            fun procRecords batch recs env =
                let
                    fun buscaEnv env' t = (case tabBusca(t, env) of
                                            SOME (x as (TRecord _)) => TTipo (t, ref (SOME x))
                                            |SOME t' => t'
                                            |_ => (case List.find (fn {name, ...} => name = t) recs of
                                                        SOME {name, ...} => TTipo (name, ref NONE)
                                                        |_ => error(t^"*no existe", 666)))
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
                        |precs _ _ = error("error internoo", 666)
                in 
                    precs batch (fromTab env)
                end
                
            fun fijaNONE [] env = env
                |fijaNONE ((name, TArray (TTipo (s, ref NONE), u)) :: t) env =
                    (case tabBusca(s, env) of 
                        SOME (r as (TRecord _)) => fijaNONE t (tabRInserta(name, TArray (TTipo(s, ref (SOME r)), u), env))
                        |_ => error("666+1", 666))
                        
                        (*TRecord of (string * Tipo * int) list * unique*)
                        
                        
                        
                |fijaNONE ((name, TRecord (lf, u)) :: t) env =
                        let
                            fun busNONE ((s, TTipo (t, ref NONE), n), l) =
                                (case tabBusca(t, env) of
                                    SOME (tt as (TRecord _)) => (s, TTipo (t, ref (SOME tt)), n) :: l
                                    | SOME _ => error (s ^ " no record?", 666)
                                    | _ => error (s^": Tipo inexistente", 666))
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
                                  
             fun printTenv [] = print "\n"
            | printTenv ((name, _) :: tss) = (print (name^"\n"); printTenv tss)
            
            fun checkNames [] = true
            | checkNames ((x, pos)::xs) = if List.exists (fn y => x=(#1 y)) xs then error("Nombre '"^x^"' duplicado", pos)
                                          else checkNames xs  
            val decs = List.map #1 ts
            val _ = checkNames (List.map (fn (x, pos) => (#name x, pos)) ts)
            val tenv' = fijaTipos decs tenv
(*            val _ = printEnv (tabClaves tenv')*)

            (*val _ = (print("\n"); printTenv (tabAList tenv'))*)      
        
        (* 
                   El problema de los ciclos es solo para tipos q no sean records, 
                    type B = A
                    type A = C
                    type C = array of int
                    
                    se generan pares de esta forma
                    (A,B)(C,A)
                    
                    y el topsort te devuelve algo de esta forma, o tira un error si encuentra un ciclo
                    [C, A, B]
                    
                    Para los tipos recursivos es q tenemos q meterlos con referencia NONE por ejemplo
                    
                    type list = {hd:int, tl:list}
                    
                    metemos list como un TTipo con ref NONE, 
                    luego para procesar el TRecord se tendra TRecord de [(string:hd, tipo:int, n),[(string:tl, tipo list, n)] 
                    de esta forma nos queda un "vacio" con NONE, luego al cambiarle la ref por un SOME algo, como le estamos cambiando el puntero
                    cada vez q encontremos un NONE, en vez de eso encontrara un SOME algo
                    
                    El pos no hay q hacerlo tan detallado... el 666 va como piña :P
                    *)
        
        in
            (venv, tenv', []) (*COMPLETAR*)
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
