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

module Set = Set.Make(Tuple)

let combine lst1 lst2 =
  let rec aux l1 l2 = match l1,l2 with
  [], _ -> []
  | e::l, [] -> aux l lst2
  | e::l, x::xs -> (e,x)::(aux (e::l) xs)
  in
  aux lst1 lst2

let foldl1 fn lst = List.fold_left fn (List.hd lst) (List.tl lst)

let getSucc size (x,y) =
    List.filter (fun (i,j) -> i > 0 && i <= size && j > 0 && j <= size && (abs
    i) <>
    (abs j))
    (List.map (fun (a,b) -> (x+a,y+b)) (combine [-2;-1;1;2] [-2; -1;1;2]))

let rec path size std nodes =
  let succLst = List.flatten (List.map (getSucc size) nodes) in
  let result  = std &&. (foldl1 (&&.) (List.map singleton succLst)) in
  if eq std result then
    result
  else
    path size result succLst

let doit n =
  let idx = make_indexes n in
  (foldl1 (&&.) (List.map singleton idx)) <=> (path n (singleton (1,1))
  [(1,1)])

let print =
  let either_print f = function
    True elt -> (print_string "True: "; f elt; print_newline ())
    | False elt -> (print_string "False: "; f elt; print_newline ()) in
  function
    | Some lst when lst <> [] ->
    begin
      print_endline "Solution: ";
      List.iter (either_print Tuple.print) lst
    end
  | _ -> print_endline "No solutions"

(** Main function *)
let _ =
  let bdd = doit 8 in
  print_int (satCount bdd); print_newline ();
  print (getSat bdd)
