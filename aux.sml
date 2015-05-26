

(* Funcion para imprimir listas, f es la funcion que imprime el tipo de dato que es la lista *)
fun printList f [] = print ""
fun printList f (x :: xs) = print (f x); (printList xs)

fun printEnv [] = print ""
    |printEnv (x::xs) = (print x; printEnv xs)

