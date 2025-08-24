let best_offer a b c =
  let compare_options x y =
    match x, y with
    | None, None -> None
    | Some x_val, None -> Some x_val
    | None, Some y_val -> Some y_val
    | Some x_val, Some y_val -> Some (max x_val y_val)
  in
  compare_options a (compare_options b c)