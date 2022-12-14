let rec total l =
  match l with
  | [] -> 0
  | h :: t -> h + total t


let rec length l =
  match l with 
  | [] -> 0
  | _ :: t -> 1 + length l

let rec append a b =
  match a with 
  | [] -> b
  | h :: t -> h :: append t b

let rec map f l = 
  match l with
  | [] -> []
  | h :: t -> f h :: map f t

exception Not_Match of string
let rec map2 f l1 l2 =
  if List.length l1 <> List.length l2
    then raise (Not_Match "Not match!")
    else begin
      match l1, l2 with
      | [], [] -> []
      | h1 :: t1, h2 :: t2 -> f h1 h2 :: map2 f t1 t2
      | _ -> raise (Not_Match "Not Match!")
    end

