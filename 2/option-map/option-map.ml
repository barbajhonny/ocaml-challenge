let option_map f = fun x-> match x with
| Some x -> Some (f x)
| None -> None;;
