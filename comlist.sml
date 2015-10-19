(*
	[x+2 || x<- [1,2,3,4], x mod 2=0]
*)
fun mapend _ [] = []
| mapend f (h::t) = f h@mapend f t

val x = mapend (fn x => if x mod 2=0 then [x+2] else []) [1,2,3,4]


fun qs [] = []
| qs((e as (h: string, _))::t) =
  let val (m, Me) = List.partition (fn(x, _) => x<h) t
  in  qs m@[e]@qs Me end
val x = qs [("z", 10), ("q", 11), ("a", 13)]
