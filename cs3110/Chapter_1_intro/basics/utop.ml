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

