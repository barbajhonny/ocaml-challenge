From type to function
Write functions with the following types. For clarity, in the types below we have added parentheses. Recall however that the -> operator associates to the right, and that * has precedence over ->.

f1 : int -> bool
f2 : bool -> int
f3 : int -> (int * bool)
f4 : (int * bool) -> int
f5 : int -> (int -> int)
f6 : int -> (int -> bool)
f7 : bool -> (int -> bool)
f8 : bool -> (bool -> int)
f9 : bool -> (int -> int)
f10 : (int -> int) -> int
f11 : (int -> bool) -> int
f12 : (bool -> int) -> int
f13 : (int -> bool) -> bool
f14 : (bool -> bool) -> int
f15 : int -> (int * int) -> int
f16 : int -> (int -> (int -> int))
f17 : (int -> int) -> (int -> int)
f18 : ((int -> int) -> int) -> int
f19 : (int -> int) -> (bool -> bool)
f20 : (int -> bool) -> (bool -> int)