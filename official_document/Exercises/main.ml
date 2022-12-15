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
  
(* Run-length encoding of a list (direct solution) *)

let encode list =
  let rle count x = if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> rle count x :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 (rle count a :: acc) t
  in
    List.rev (aux 0 [] list)

(* Duplicate the elements of a list *)
let duplicate lst =
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (h :: h :: acc) t
in aux [] (List.rev lst)


(* Replicate the elements of a list a given number of times *)

let replicate lst n = 
  let rec rep_1 ele acc n =
    if n = 0 then acc
    else rep_1  ele (ele :: acc) (n - 1)
  in
  let rec aux acc n =
    function
    | [] -> acc
    | h :: t -> aux (rep_1 h acc n) n t 
  in aux [] n lst

(* Drop every N'th element from a list *)
let drop lst n =
  let rec aux acc cur_num n = function
  | [] -> acc
  | h :: t -> begin
    if cur_num mod n = 0 
      then aux acc (cur_num + 1) n t
  else aux (h :: acc) (cur_num + 1) n t
end
in List.rev (aux [] 1 n lst)

let drop list n =
  let rec aux i = function
  | [] -> []
  | h :: t -> if i = n then aux 1 t else h :: aux (i + 1) t in
  aux 1 list


(* Split a list into two parts; the length of the first part is given *)
let split lst n =
  let rec aux acc cur_num n = function
  | [] -> [List.rev acc, []]
  | h :: t -> begin
    if cur_num = n then
      [List.rev (h::acc), t]
    else
      aux (h::acc) (cur_num + 1) n t
    end
  in 
  assert (n > 0);
  aux [] 1 n lst

let split list n = 
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l -> if i = 0 then List.rev acc, l
    else aux (i - 1) (h :: acc) t
  in aux n [] list

(* Extract a slice from a list *)
let slice lst start finish = 
  let lst_size = List.length lst in
  assert (start < lst_size);
  assert (finish < lst_size);
  assert (start <= finish);
  let rec aux acc pos start finish lst = 
    if pos < start then
      aux acc (pos + 1) start finish lst
    else
      if pos > finish then
        acc
      else
        aux ((List.hd lst):: acc) (pos + 1) start finish (List.tl lst)
      in aux [] 0 start finish lst

let slice list i k =
  let rec take n = function 
  | [] -> []
  | h :: t -> if n = 0 then [] else h :: take (n - 1) t
in
let rec drop n = function 
  | [] -> []
  | h :: t as l -> if n = 0 then l else drop (n - 1) t
in take (k - i + 1) (drop i list)

let rec fold_until f acc n = function
  | [] -> (acc, [])
  | h :: t as l -> if n = 0 then (acc, l)
  else fold_until f (f acc h) (n - 1) t

let slice list i k =
  let _, list = fold_until (fun _ _ -> []) [] i list in
  let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
  List.rev taken

(* Rotate a list N places to the left *)

let rotate lst n =
    let first, second = split lst n in
    List.rev (
      List.append (List.rev first) (List.rev second)
    )

let split list n =
  let rec aux i acc = function
  | [] -> List.rev acc, []
  | h :: t as l -> if i = 0 then List.rev acc, l
  else aux (i - 1) (h :: acc) t in
  aux n [] list

let rotate list n =
  let len = List.length list in
  let n = if len = 0 then 0 else (n mod len + len) mod len in
  if n = 0 then list
  else let a, b = split list n in b @ a


(* Remove the K'th element from a list *)

let remove_at k lst = 
  let rec aux acc k pos lst = 
    if pos = k
      then aux acc k (pos + 1) (List.tl lst)
  else
    match lst with
    | [] -> acc
    | h :: t -> aux (h :: acc) k (pos + 1) t
  in List.rev (aux [] k 0 lst)