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

(** Syntactic sugar over foldl1. The accumulator part of fold_left is
 * the head of the list *)
let foldl1 fn lst = List.fold_left fn (List.hd lst) (List.tl lst)

(** Checks whether the coordinates (i,j) are in Line of sight of the queen
 * at (x,y) *)
let invalid (x,y) (i,j) = ((x,y) <> (i,j))
    && ((x+y==i+j) || (x-y==i-j) || (x==i) || (y==j))

(* For a given variable at (x,y) and a board size n,
 * does the implication between (x,y) an the celles in its line of sight *)
let in_sight n (x,y) =
  let idx = make_indexes n in
  let cannot_exist = foldl1 (&&.)
    (List.map (fun a -> neg (singleton a))
      (List.filter (invalid (x,y)) idx)) in
  (singleton (x,y)) => cannot_exist

(** Base function that generates the ROBDD representing the N queens problem *)
let doit n =
  let seqs = (make_seq n) in
  let queens = List.fold_left
      (fun acc i -> acc &&. (List.fold_left
        (fun acc j -> (singleton (i,j)) ||. acc) Zero seqs))
        One seqs
  in
  let idx = make_indexes n in
  List.fold_left (&&.) queens (List.map (in_sight n) idx)

let print =
  let either_print f = function
    True elt -> (print_string "True: "; f elt; print_newline ())
    | False elt -> (print_string "False: "; f elt; print_newline ()) in
  function
  None -> print_endline "No solutions"
  | Some lst ->
    begin
      List.iter (either_print Tuple.print) lst
    end

(** Main function *)
let _ =
  let bdd = doit 9 in
  print_int (satCount bdd); print_newline ();
  print (getSat bdd)
