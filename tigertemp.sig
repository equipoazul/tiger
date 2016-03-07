signature tigertemp = sig
	type label = string
	type temp = string
	val makeString: string -> string
	val newtemp: unit -> temp
	val newlabel: unit -> label
    val string2temp: string -> temp
    val temp2string: temp -> string 
end
