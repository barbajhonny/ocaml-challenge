let (<|>) x y  = match x with
| Some x -> Some x
| None-> y;;
 

assert (None <|> Some false = Some false);;
assert (Some true <|> None <|> Some false = Some true);;
assert (Some 3 <|> None = Some 3);;
assert (Some "cat" <|> Some "dog" = Some "cat");;
assert (None <|> None <|> Some "dog" <|> None = Some "dog");;
assert (None <|> None <|> None <|> Some "dog" <|> None = Some "dog");;