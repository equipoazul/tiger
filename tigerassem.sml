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

		val format =
			let	fun speak(assem,dst,src,jump) =
					let	val saylab = tigertab.name
					    fun saytemp t = t
						fun f(#"`":: #"s":: i::rest) = 
							(explode(saytemp(List.nth(src,ord i - ord #"0")))
								@ f rest)
						| f(#"`":: #"d":: i:: rest) = 
							(explode(saytemp(List.nth(dst,ord i - ord #"0")))
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

