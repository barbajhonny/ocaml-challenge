let double x = x * 2;;
let square x = x * x;;

let comp s d = fun x -> s(d(x));;

assert((comp square double) 3 = 36);;
assert((comp double square) 3 = 18);;

assert(comp (fun b -> if b then 0 else 1) (fun x -> x>0) 3 = 0);;
assert(comp (fun b -> if b then 0 else 1) (fun x -> x>0) 0 = 1);;

