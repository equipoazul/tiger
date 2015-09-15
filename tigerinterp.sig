signature tigerinterp =
sig
 
val inter : bool -> ((tigertree.stm list*tigerframe.frame) list) -> ((tigertemp.label*string) list)-> unit

end
