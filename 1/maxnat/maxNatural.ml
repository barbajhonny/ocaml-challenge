let always_fail x = failwith("this function always fails")

let max_number a b = if a>b then a else b

let max_nat a b = if a < 0 || b < 0 then always_fail b else max_number a b;;


(*assert(max_nat 2 5 = 5);;
assert(max_nat 5 2 = 5);;
assert(try max_nat (-2) 5 |> fun _ -> false with _ -> true);;
assert(try max_nat 2 (-5) |> fun _ -> false with _ -> true);;
assert(try max_nat (-2) (-5) |> fun _ -> false with _ -> true);;*)