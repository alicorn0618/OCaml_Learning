(* Lists *)
(* syntax of lists
   e1 :: e2 
   [e1; e2; e3; ...] *)
[];;

[[[]]; [[1; 2; 3]]];;

let rec sum lst = 
  match lst with
  | [] -> 0
  | h :: t -> h + sum t;;

let sum lst =
  let rec sum_aux acc lst =
    match lst with
    | [] -> acc
    | h :: t -> sum_aux (acc + h) t
  in sum_aux 0 lst;;

let sum xs = 
  let rec sum_aux acc xs = 
    match xs with
    | [] -> acc
    | x :: xs' -> sum_aux (acc + x) xs'
  in sum_aux 0 xs;;

let rec length lst =
  match lst with
  | [] -> 0
  | _ :: t -> 0 + length t;;

List.length [];;

List.append [1; 2; 3] [4; 5; 6];;

let empty lst = 
  match lst with
  | [] -> true
  | _ -> false;;

let inc_first lst =
  match lst with
  | [] -> []
  | h :: t -> (h + 1) :: t;;

(* pattern matching with lists *)
(* match e with
  | p1 -> e1
  | p2 -> e2
  | ...
  | pn -> en
  *)

match 1 :: [] with
  | [] -> false
  | h :: t -> h >= 1 && List.length t = 0;;

let head lst = match lst with
  | [] -> None
  | h :: t -> Some h;;

let length_is lst n = 
  match lst with
  | [] -> false
  | _ :: t -> List.length t = n - 1;;

let rec from i j l = if i > j then l else from i (j - 1) (j :: l);;
let ( -- ) i j = from i j [];; 

let long_list = 0 -- 1_000_000;;

List.init 1_000_000 Fun.id;;

(* Variant *)
type day = 
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun;;

let d = Tue;;

let int_of_day (d:day) =
  match d with
  | Mon -> 1
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  | Sun -> 7;;

(* syntax of variant type
    type t = 
      | C1 of t1
      | C2 of t2
      | ...
      | Cn of tn
*)

(* scope *)
type t1 = C | D;;
type t2 = D | E;;
let x = D;;

type ptype =
  TNormal | TFire | TWater | TGrass | TElectric | TPsychic;;

type peff = 
  | ENormal
  | EFire
  | EWater
  | EGrass
  | EElectric
  | EPsychic
  | ENotVery
  | ESuper;;


type ptype = TNormal | TFire | TWater;;
type mon = {name : string; hp : int; ptype : ptype};;

{name = "Charmander"; hp = 39; ptype = TFire};;

let c = {name = "Charmander"; hp = 39; ptype = TFire};;

c.hp;;

match c with
| {name = n; hp = h; ptype = t} -> h;;

match c with {name; hp; ptype} -> hp;;

(* a record expression is written:
   {f1 = e1; ...; fn = en}*)

(*record copy*)
let c' = {c with hp = 40};;

(* record field punning *)
let c' = {c with hp = c.hp + 1};;

(* record field update *)
let c' = {c with hp = c.hp + 1};;

(* tuples *)
(1, 2, 10);;
(true, "Hello");;
(1, (2, 3));;
(1, 2, 3) = (1, 2, 3);;

let (a, b, c) = (1, 2, 3);;
a;;

match (1, 2, 3) with (x, y, z) -> x + y + z;;

(* Advanced Pattern Matching *)


type ptype = TNormal | TFire | TWater;;

type mon = {name : string; hp : int; ptype : ptype};;

let get_hp m = match m with {name = n; hp = h; ptype = t} -> h;;

let get_hp m = match m with {name = _; hp = h; ptype = _} -> h;;

let get_hp m = match m with {name; hp; ptype} -> hp;;
let get_hp m = match m with {hp} -> hp;;
let get_hp m = m.hp;;

let fst (x, _) = x;;
let snd (_, y) = y;;

let thrd t = match t with x, y, z -> z;;

let thrd t = let x, y, z = t in z;;

let thrd t (_, _, z) = z;;

type point = float * float;;
type vector = float list;;
type matrix = vector list;;

let get_x = fun (x, _) -> x;;

let p1 : point = (1.0, 2.0);; 
let p2 : float * float = (1.0, 2.0);;

let a = get_x p1;;
let b = get_x p2;;

(* Options *)
let rec list_max = function
  | [] -> None
  | h :: t -> max h (list_max t);;

Some 42;;

None;;

let extract o =
  match o with
  | Some i -> string_of_int i
  | None -> "";;

extract (Some 42);;
extract None;;

(* Association Litts *)
let d = [
  ("rectangle", 4);
  ("triangle", 3);
  ("icosahedron", 20)
]

let insert k v lst = (k, v) :: lst;;

let rec lookup k lst =
  match lst with
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t;;

(*Algerbaic Data Tyes*)

type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat;;

type point = float * float;;

type shape =
  | Point of point
  | Circle of point * float
  | Rect of point * point;;

let area = function
  | Point _ -> 0.0
  | Circle (_, r) -> Float.pi *. r *. r
  | Rect ((x1, y1), (x2, y2)) ->
      let w = x2 -. x1 in
      let h = y2 -. y1 in
      w *. h;;

let center = function
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2)) ->
      ((x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0);;

type string_or_int = 
    | String of string
    | Int of int;;

type string_or_int_list = string_or_int list;;

let rec sum : string_or_int_list -> int = function
  | [] -> 0
  | String s :: t -> sum t
  | Int i :: t -> i + sum t;;

let lst_sum = sum [String "1"; Int 2];;

type t = Left of int | Right of int;;
let x = Left 1;;
let double_right = function
  | Left i -> Left i
  | Right i -> Right (2 * i);;

(*
   to define
   type t = C1 [of t1] | C2 [of t2] | ... | Cn [of tn]
   to write
   [C e] or [C]
*)
(* Catch-all Cases *)
type color = Blue | Red;;

let string_of_color = function
    | Blue -> "blue"
    | _ -> "red";;

type color = Blue | Red | Green;;

(* Recursive Variants *)

type intlist = Nil | Cons of int * intlist;;

let lst3 = Cons (3, Nil);;

let lst123 = Cons (1, Cons (2, lst3));;

let rec sum (l:intlist) : int =
  match l with
  | Nil -> 0
  | Cons (h, t) -> h + sum t;;

let rec length (l:intlist) : int =
  match l with
  | Nil -> 0
  | Cons (_, t) -> 1 + length t;;


let empty : intlist -> bool = function
  | Nil -> true
  | _ -> false;;


type node = {value : int; next : mylist}
and mylist = Nil | Cons of node;;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

let lst3 = Cons (3, Nil);; (* Similar to lst3 = 3 :: [] *)
let lst_hi = Cons ("hi", Nil);; (* Similar to lst_hi = "hi" :: [] *)

let rec length : 'a mylist -> int = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t;;

let empty : 'a mylist -> bool = function
  | Nil -> true
  | _ -> false;;


let rec sum = function
  | Nil -> 0
  | Cons (h, t) -> h + sum t;;

type ('a, 'b) pair = {fst : 'a; snd : 'b};;
let x = {fst = 3; snd = "hi"};;

(* Polymorphic Variants *)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat;;

type shape = 
  | Point of point
  | Circle of point * float
  | Rect of point * point;;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

type fin_or_inf = Finite of int | Infinity;;

