signature tigerframe =
sig

type frame
type register = string
val rv : tigertemp.temp
val ov : tigertemp.temp
val fp : tigertemp.temp
datatype access = InFrame of int | InReg of tigertemp.label
val fpPrev : int
val fpPrevLev : int
val newFrame : {name: tigertemp.label} -> frame
(* Esta funcion agrega los accces al frame alojado en el level que le pasamos *)
val addAccFrame : access -> frame -> unit 
val name : frame -> tigertemp.label
val string : tigertemp.label * string -> string
val globl : string -> string
val formals : frame -> access list
val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access
val sp : tigertemp.temp
val maxRegFrame : frame -> int
val wSz : int
val log2WSz : int
val argregs : tigertemp.temp list
val calldefs : tigertemp.temp list
val callersaves : tigertemp.temp list
val calleesaves : tigertemp.temp list
val exp : access -> tigertree.exp
val externalCall : string * tigertree.exp list -> tigertree.exp
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
val procEntryExit3 : frame * tigerassem.instr list -> tigerassem.instr list
val printAccess: access -> unit
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string

end
