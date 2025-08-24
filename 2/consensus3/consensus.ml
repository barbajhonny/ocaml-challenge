

let consensus3 (f1, f2, f3) = 
fun x -> match (f1 x, f2 x, f3 x) with
  | (v1, v2, v3) -> 
      if v1 = v2 && v2 = v3 then Some v1
      else if v1 = v2 then Some v1
      else if v1 = v3 then Some v1
      else if v2 = v3 then Some v2
      else None
;;


assert(consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 1 = Some 5);;
assert(consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 2 = Some 2);;
assert (consensus3 ((fun x -> x), (fun y -> y+4), (fun z -> 5/z)) 3 = None);;