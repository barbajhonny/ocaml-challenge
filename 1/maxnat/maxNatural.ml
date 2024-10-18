
let always_fail x = failwith "This function always fails"
let maxNumber a b = if a > b then a else b
let max_nat a b = if a <= 0 || b <= 0 then always_fail 0 else maxNumber a b
