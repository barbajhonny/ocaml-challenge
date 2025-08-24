let p x = x > 0;;
let q x = x mod 2 = 0;;


let both_true p q =
  fun x -> (p(x) && q(x));;

let is_positive_and_even = both_true p q;;
assert(is_positive_and_even 4 = true);;
assert(is_positive_and_even 3 = false);;