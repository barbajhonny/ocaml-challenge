let is_even x = x mod 2 = 0

let win a b =
  let correct x = x >= 1 && x <= 5 in
  let a_correct = correct a in
  let b_correct = correct b in
  match (a_correct, b_correct) with
  | (true, true) -> if is_even (a + b) then 1 else -1
  | (true, false) -> 1
  | (false, true) -> -1
  | _ -> 0