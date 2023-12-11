
7 * (1 + 2 + 3);;

"CS" ^ string_of_int 3110;;

42 / 10;;
3.14 /. 2.;;


(* = means structural equality, == means physical equality *)
(* <> means not equal, != means not equal *)
(* assert means if the expression is true, then do nothing, otherwise raise an exception *)
(* assert false means raise an exception *)
(* assert true means do nothing *)
42 == 42;;
"CS" == "CS";;
"CS" = "CS";;

assert true;;
assert false;;

assert (2110 <> 3110);;

let _ = if 2 > 1 then 42 else 7;;

let double x = x * 2;;
assert (double 21 = 42);;
assert (double 32 = 64);;

let cube x = x ** 4.;;
(* assert ((cube 2.) - 16.) <. 1e-10;; *)
assert (cube 3. = 81.);;
assert (cube 4. = 256.);;

let sign n = if n > 0 then 1 else (if n < 0 then -1 else 0);;
assert (sign 42 = 1);;
assert (sign (-42) = -1);;
assert (sign 0 = 0);;

let avg3 x y z = (x +. y +. z) /. 3.;;

let rms x y = sqrt ((x ** 2. +. y ** 2.) /. 2.);;
assert (rms 1. 1. = 1.);;

let date (d:int) (m : string) = 
  match m with
  | "Jan" | "Mar" | "May" | "Jul" | "Aug" | "Oct" | "Dec" -> d <= 31 && d > 0
  | "Apr" | "Jun" | "Sep" | "Nov" -> d <= 30 && d > 0
  | "Feb" -> d <= 28 && d > 0
  | _ -> false;;

let rec fib n = 
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2);;

let fast_fib n = 
  if n < 0 then invalid_arg "n must be non-negative"
  else
    let rec aux n a b =
      match n with
      | 0 -> a
      | 1 -> b
      | _ -> aux (n - 1) b (a + b)
    in aux n 0 1;;

let f x = if x then x else x;;
let g x y = if y then x else x;;

let h x y z = if x then y else z;;
let i x y z = if x then y else y;;

let divide ~numerator:(n:float) ~denominator:(d:float) = n /. d;;

let add = fun x -> fun y -> x + y;;

add 5 1;;
add 5;;
(add 5) 1;;

print_endline "Hello, world";;
print_string "Hello, world";;