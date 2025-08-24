let add y= Some(y+1)

let incr_opt x = match x with
| None  -> None
| Some (x) ->  add x
