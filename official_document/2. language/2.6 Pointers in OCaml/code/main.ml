type list = Nil | Cons of int * list

type list = Nil | Cons of cell
  and cell = {mutable hd : int; tl : list}

type list = Nil | Cons of cell
  and cell = {mutable hd: int; mutable tl: list}

type 'a pointer = Null | Pointer of 'a ref

let (!^)  = function
  | Null -> invalid_arg "Attempt to dereferencing the null pointer"
  | Pointer r -> !r

let ( ^:= ) p v =
  match p with
  | Null -> invalid_arg "Attempt to assign the null pointer"
  | Pointer r -> r := v

let new_pointer x = Pointer (ref x)

let p = new_pointer 0

let _ = p ^:= 1

let _ = !^p

type ilist = cell pointer
  and cell = {mutable hd: int; mutable tl: ilist}

