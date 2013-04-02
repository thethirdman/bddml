open Printf
(** We first declare the structure of the variables handled by the
 * ROBDD: t represents 2 dimensional coordinates *)
module Tuple =
  struct
    type t = int * int
    let compare = Pervasives.compare
    let print (a,b) = printf "(%d,%d)" a b
  end

module M = Bdd.BDD(Tuple)
open M

(** List generation funtions *)
(** In 1d *)
let rec make_seq = function
  0 -> []
  | n -> n::(make_seq (n-1))

(** And 2d *)
let make_indexes n =
  let rec sub n = function
    (i,j) when i > n -> []
    | (i,j) when j == n -> (i,j)::(sub n (i+1,1))
    | (i,j) -> (i,j)::(sub n (i,j+1)) in
  sub n (1,1)

let valid_move size (x,y) (i,j) =
  ((abs i) <> (abs j))
  && (x + i) > 0 && (x + i) <= size
  && (y + j) > 0 && (y + j) <= size

let getSucc size (i,j) =
  let lst = [-2;-1;1;2]
  in
  let rec aux l1 l2 = match l1,l2 with
  [], _ -> []
  | e::l, [] -> aux l lst
  | x::xs, y::ys ->
      if valid_move size (i,j) (x,y) then
        (x+i,y+j)::(aux (x::xs) ys)
      else
        (aux (x::xs) ys)
  in
  aux lst lst

let foldl1 fn lst = List.fold_left fn (List.hd lst) (List.tl lst)

let rec flattenmap f = function
  [] -> []
  | e::l -> (f e)@(flattenmap f l)

let rec uniq lst =
  let rec rm_dup elt = function
    [] -> []
    |  e::l when elt == e -> rm_dup elt l
    | e::l -> e::(rm_dup e l) in
  let sorted = List.sort Tuple.compare lst in
  rm_dup (List.hd sorted) (List.tl sorted)

let rec path size std nodes =
  let succLst = uniq (flattenmap (getSucc size) nodes) in
  let result  = std &&. (foldl1 (&&.) (List.map singleton succLst)) in
  if eq std result then
    result
  else
    path size result succLst

let doit n =
  let idx = make_indexes n in
  (foldl1 (&&.) (List.map singleton idx)) <=> (path n (singleton (1,1))
  [(1,1)])

let usage () =
  print_string ("Usage:" ^ Sys.argv.(0) ^ " N")

(** Main function *)
let _ =
  try
    let bdd = doit (int_of_string (Sys.argv.(1))) in
      match bdd with
      One -> print_endline "All of the cells can be visited"
      |_ -> print_endline "Some cells are unreachable"
  with
  _ -> usage ()
