(* This is my ocaml exercises, my solution, then official solution *)
(* Tail of a list *)

(* Tail of a list *)
let rec last_tr lst last = 
  match lst with
  | [] -> last
  | h :: t -> last_tr t (Some h)

let last l = last_tr l None

(*
 let rec last = function 
 | [] -> None
 | [ x ] -> Some x
 | _ :: t -> last t
*)

(* Last two elements of a list *)
let rec last_two lst =
  match lst with
  | [] -> None
  | [a] -> None
  | [a; b] -> Some (a, b)
  | _ :: t -> last_two t

(*
let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x,y)
    | _ :: t -> last_two t;;
*)

(* N'th element of a list *)

let rec nth lst n = 
  if n < 0 then raise (Failure "nth")
  else
    begin
      match lst with
      | [] -> raise (Failure "nth")
      | h :: t when n = 0 -> h
      | _ :: t -> nth t (n - 1)
    end

(*
let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t;;
   *)

(* Length of a list *)

let rec length_tr l acc =
  match l with
  | [] -> acc
  | _ :: t -> length_tr l (acc + 1)

let length l = length_tr l 0

let rec length l = 
  let rec aux lst acc = 
    match lst with
    | [] -> acc
    | _ :: t -> aux t (acc + 1)
  in aux l 0


(* Reverse a list *)
let rev lst = 
  let rec aux l1 l2 =
    match l1 with
    | [] -> l2
    | h :: t -> aux t (h :: l2)
  in aux lst []

let rev list =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (h :: acc) t
in aux [] list

(* Palindrome *)

let is_palindrome lst =
  lst = (List.rev lst)

(* Flatten a list *)

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
  | [] -> []
  | One x :: t -> aux (x :: acc) t
  | Many l :: t -> aux (aux acc l) t
in aux [] lst

(* Eliminate duplicates *)

let rec compress = function
  | a :: (b :: c as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

let rec compress = function
  | a :: (b :: c as t) -> if a = b then compress t else a :: compress t
  | smaller -> smaller

(* Pack consecutive duplicates *)

let pack lst = 
  let rec aux cur acc = function
  | [] -> []
  | [x] -> (x :: cur) :: acc
  | a :: (b :: c as t) -> begin
     if a = b then aux (a :: cur) acc t
     else aux [] ( (a :: cur) :: acc) t
    end
  in aux [] [] lst

(* Run-length encoding *)

let encode lst =
  let rec aux cur_len acc = function
  | [] -> []
  | [x] -> (cur_len + 1, x) :: acc
  | a :: (b :: c as t) -> if a = b then
    aux (cur_len + 1) acc t
  else aux 0 ((cur_len + 1, a) :: acc) t
in aux 0 [] lst

(* Modified run-length encoding *)
type 'a rle = 
  | One of 'a
  | Many of int * 'a

let encode lst = 
  let rec aux cur_len acc = function
  | [] -> []
  | [x] ->  if cur_len = 0 then (One x) :: acc else (Many (cur_len + 1, x)) :: acc
  | a :: (b :: c as t ) -> if a = b then aux (cur_len + 1) acc t else begin
    if cur_len = 0 then aux 0 (One a :: acc) t else aux 0 (Many (cur_len + 1, a) :: acc) t
  end
in aux 0 [] (List.rev lst)

let encode l =
  let create_tuple cnt elem =
    if cnt = 1 then One elem
    else Many (cnt, elem) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_tuple (count + 1) x) :: acc
    | hd :: (snd :: _ as tl) ->
        if hd = snd then aux (count + 1) acc tl
        else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
    List.rev (aux 0 [] l)

(* Decode a run-length encoded list *)
let decode lst =
  let rec aux acc = function
  | [] -> []
  | h :: t -> begin
    match h with
    | One s -> s :: acc
    | Many (2, s) -> aux (s :: acc) ((One s) :: t)
    | Many (n, s) -> aux (s :: acc) ((Many (n-1, s))::t )
  end
in List.rev (aux [] lst)


let decode list =
  let rec many acc n x = 
    if n = 0 then acc else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
  | [] -> acc
  | One x :: t -> aux (x :: acc) t
  | Many (n, x) :: t -> aux (many acc n x) t
in
  aux [] (List.rev list)  