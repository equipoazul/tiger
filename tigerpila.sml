structure tigerpila :> tigerpila =
struct

type 'a Pila = 'a list ref
fun nuevaPila() = ref []
fun nuevaPila1 e = ref [e]
fun pushPila pila item = pila:=(item::(!pila))
fun popPila pila =
	let	val ret = hd(!pila)
	    val _ = print("Saque algo de la pila\n")
	in pila:=tl(!pila) end
fun topPila pila = hd(!pila)

end
