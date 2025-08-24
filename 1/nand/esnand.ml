(*Logic port with connectives*)
let nand a b = not (a && b);;

(*logic port with conditional expression*)
let nand1 c d = if (c = true && d =true) then false else true;;

(*Logic port with pattern matching*)
let nand2 e f =
  match e, f with
  | false, false -> true
  | false, true -> true
  | true, false -> true
  |true, true -> false;;