(*Logic port with connectives*)
let xor a b = (a && (not b)) || ((not a) && b)

(*logic port with conditional expression*)
let xor1 c d = if c=d then false else true

(*Logic port with pattern matching*)
let xor2 e f = 
  match (e, f) with
  | (false, false) -> false
  | (false, true) -> true
  | (true, false) -> true
  | (true, true) -> false
