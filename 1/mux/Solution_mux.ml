(*solution with logical connectives*)
let mux2 s0 a b = (s0 && a) || ((not s0) && b)

(*solution with conditional expression*)
let mux2a s0 a b = if s0 then a else b

(*solution with pattern matching*)
let mux2b s0 a b = match s0 with
| true -> a
| false -> b 
;;

let mux4 s1 s0 a0 a1 a2 a3 = 
  match (s1,s0) with
  | (false, false) -> mux2b false a1 a0 
  | (false , true) -> mux2b true a1 a0
  | (true, false) -> mux2b false a0 a2
  | ( true, true) -> mux2b true a3 a2 ;;
  
  
assert(mux4 false false false true false true = false);;
assert(mux4 false true false true false true = true);;
assert(mux4 true false false true false true = false);;
assert(mux4 true true false true false true = true);;