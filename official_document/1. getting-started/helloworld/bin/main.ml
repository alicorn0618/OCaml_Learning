include Core

open Core

type expr =
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Var of string


let rec to_string (e: expr) : string = 
  match e with
  | Plus (left, right) ->
     "(" ^ to_string left ^ " + " ^ to_string right ^ ")"
  | Minus (left, right) ->
     "(" ^ to_string left ^ " - " ^ to_string right ^ ")"
  | Times (left, right) ->
   "(" ^ to_string left ^ " * " ^ to_string right ^ ")"
  | Divide (left, right) ->
   "(" ^ to_string left ^ " / " ^ to_string right ^ ")"
  | Var v -> v

let print_expr (e: expr) =
  print_endline(to_string e)

let _ = print_expr(Times (Var "n", Plus (Var "x", Var "y")))

let rec multiply_out e =
  match e with
  | Times (e1, Plus (e2, e3)) ->
     Plus (Times (multiply_out e1, multiply_out e2),
           Times (multiply_out e1, multiply_out e3))
  | Times (Plus (e1, e2), e3) ->
     Plus (Times (multiply_out e1, multiply_out e3),
           Times (multiply_out e2, multiply_out e3))
  | Plus (left, right) ->
     Plus (multiply_out left, multiply_out right)
  | Minus (left, right) ->
     Minus (multiply_out left, multiply_out right)
  | Times (left, right) ->
     Times (multiply_out left, multiply_out right)
  | Divide (left, right) ->
     Divide (multiply_out left, multiply_out right)
  | Var v -> Var v



let actorize e =
  match e with
  | Plus (Times (e1, e2), Times (e3, e4)) when e1 = e3 ->
     Times (e1, Plus (e2, e4))
  | Plus (Times (e1, e2), Times (e3, e4)) when e2 = e4 ->
     Times (Plus (e1, e3), e4)
  | e -> e;;

type t' = Int of int | Add of t * t and t = {annotation: string; data: t}
type t = A | B of t' and t' = C | D of t
let rec sum_t' = function
  | Int i -> i
  | Add (i, i') -> sum_t i + sum_t i'
  and sum_t {annotation; data} =
    if annotation <> "" then Printf.printf "Touching %s\n" annotation;
    sum_t' data;;

type t = T of int * int

