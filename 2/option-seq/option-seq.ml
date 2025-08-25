let (<*>) x y = match x with
|None-> None
|Some f-> match y with
  |None -> None
  |Some k -> Some (f k);;
