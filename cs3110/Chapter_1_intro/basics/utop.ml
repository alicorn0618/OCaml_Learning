let increment (x: int) : (int -> int) =
  fun (y: int) -> x + y;;

let _ = 65 / 60;;

let _ = 65 mod 60;;

(* let _ = 65 / 0;; *)

let _ = 3.;;


let _ = 3;;

3.14 *. 2.;;

3.14 *. (float_of_int 2);;

"abc" ^ "def";;

string_of_int 42;;

String.make 1 'z';;

int_of_string "123";;

"abc".[0];;

"abc".[1];;

"abc".[2];;

(** =  and <> examines structural equality *)
(** == and != examines physical equality *)

let _ = 1 = 1;;

assert (1 = 1);;


if 3 + 5 > 2 then "yay!" else "boo!";;

4 + (if 'a' = 'b' then 1 else 2);;


(* if e1 then e2
    else if e3 then e4
    else e5
*)

let x = 42;;

let x = 42 in x + 1;;

(let x = 42 in x) + 1;;

let a = "big" in 
let b = " red" in
let c = a ^ b in
    c;;

(**syntax of let expression: 
    let <var> = <expr1> in <expr2>
*)

let x = 42 in
    x + (let y = "3110" in
        int_of_string y);;

let x = 5 in
    ((let x = 6 in 6) + 5);;


let x = 5;;

let x = "big";;
let b = "red";;

let x = 6 in x;;
let y = 6 in y;;

let x = 5 in (let x = 6 in x) + x;;


let x = 5 in ((let x = 6 in x) + x);;

let x = 5 in (x + (let x = 6 in x));;

(5 : int);;

(5. : float) +. 1.1;;

(** 2.4 Functions *)

let x = 42;;

let rec fact n = if n = 0 then 1 else n * fact (n - 1);;

let rec pow x y = if y = 0 then 1 else x * pow x (y - 1);;

let rec even n =
  n = 0 || odd (n - 1)
and odd n =
  n <> 0 && even (n - 1);;


let inc x = x + 1;;
let inc = fun x -> x + 1;;

(** pipeline *)
let square x = x * x;;

square (inc 5);;
5 |> inc |> square;;

5 |> inc |> square |> inc |> inc |> square;;
square (inc (inc (square (inc 5))));;


(** Polymorphic Functions*)

let id x = x;;

let id = fun x -> x;;

id 42;;
id true;;
id "bigred";;

let id_int (x: int) : int = x;;

let id_int' : int -> int = id;;

let first x y = x;;
let firt_int : int -> 'b -> int = first;;
String.sub;;

let f ~name1:arg1 ~name2:arg2 = arg1 + arg2;;

f ~name2:3 ~name1:4;;

let f ~name1:(arg1 : int) ~name2:(arg2 : int) = arg1 + arg2;;

let f ?name:(arg1=8) arg2 = arg1 + arg2;;

f ~name:2 7;;
f 7;;

let add x y = x + y;;
let addx x = fun y -> x + y;;

let add5 = addx 5;;
let add5 = add 5;;

let add x y = x + y;;
let add x = fun y -> x + y;;
let add = fun x -> (fun y -> x + y);;

(* Every OCaml function takes exactly one argument. *)
( + );;

let add3 = ( + ) 3;;
add3 2;;

let ( ^^ ) x y = max x y;;

let rec count n = 
  if n = 0 then 0 else 1 + count (n - 1);;


let rec count_tl n sum = match n with
| 0 -> sum
| _ -> count_tl (n - 1) (sum + 1);;

let count n = count_tl n 0;;

let rec fact n = match n with
  | 0 -> 1
  | _ -> n * fact (n - 1);;

let fact n = let rec fact_aux n sum =
  match n with
  | 0 -> sum
  | _ -> fact_aux (n - 1) (sum * n)
in fact_aux n 1;;

(* #require "zarith.top";;

let rec zfact_aux n acc =
  match n with
  | Z.equal n Z.zero -> acc
  | _ -> zfact_aux (Z.pred n) (Z.mul acc n);;

let zfact n = zfact_aux n Z.one;; *)

(** @author hugh (Dennis.Chen@sjtu.edu.cn)*)

(** Sum a list.
    @param [lst] is the List to be summed
    @return the sum of the list *)
let rec sum lst = match lst with   
  | [] -> 0
  | h::t -> h + sum t;;

let () = print_endline "Camels" in
let _ = print_endline "are" in
print_endline "bae";;

print_endline "Camels";
print_endline "are";
print_endline "bae";;


let print_stat name num = 
  Printf.printf "%s: %f\n%!" name num;;
(* %! means to flush the output buffer *)

let rec fib x = if x < 2 then 1 else fib (x - 1) + fib (x - 2);;

( lsl );