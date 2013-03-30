open Printf
module Tuple =
  struct
    type t = int * int
    let compare = Pervasives.compare
    let print (a,b) = printf "(%d,%d)" a b
  end

module M = Bdd.BDD(Tuple)
open M

let rec make_seq = function
  0 -> []
  | n -> n::(make_seq (n-1))

let make_indexes n =
  let rec sub n = function
    (i,j) when i > n -> []
    | (i,j) when j == n -> (i,j)::(sub n (i+1,1))
    | (i,j) -> (i,j)::(sub n (i,j+1)) in
  sub n (1,1)

let foldl1 fn lst = List.fold_left fn (List.hd lst) (List.tl lst)

let invalid (x,y) (i,j) = ((x,y) <> (i,j))
    && ((x+y==i+j) || (x-y==i-j) || (x==i) || (y==j))

let in_sight n (x,y) =
  let idx = make_indexes n in
  let cannot_exist = foldl1 (&&.)
    (List.map (fun a -> neg (singleton a))
      (List.filter (invalid (x,y)) idx)) in
  (singleton (x,y)) => cannot_exist

let doit n =
  let seqs = (make_seq n) in
  let queens = List.fold_left
      (fun acc i -> acc &&. (List.fold_left
        (fun acc j -> (singleton (i,j)) ||. acc) Zero seqs))
        One seqs
  in
  let idx = make_indexes n in
  List.fold_left (&&.) queens (List.map (in_sight n) idx)

let print = function
  None -> print_endline "No solutions"
  | Some lst -> begin print_int (List.length lst); List.iter Tuple.print lst end

let _ =
  let bdd = doit 8 in
  print (getSat bdd)
