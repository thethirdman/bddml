(** The BDD module expects a functor that describes the Key representing
 * the BDD (Int, String, Tuple, ...).
 *)
module BDD =
  functor (Type :
    sig
      type t
      val compare : t -> t -> int
    end) ->
struct

(** ID PART *)
type uid = int

(** Uid generator (lexical closure over a lambda function) *)
let uidMake =
let cur = ref 2 in
  (fun () -> (incr cur); !cur)

(** BDD PART *)
type 'a bdd = One | Zero
  | Robdd of ('a bdd) * 'a * ('a bdd) * uid
  | Robddref  of uid * 'a * uid * uid

(* Takes a node an return its identifier *)
let identifier = function
  | One -> 1
  | Zero -> 0
  | Robdd (_,_,_,id) -> id
  | Robddref (_,_,_,id) -> id

let eq a b = (identifier a) == (identifier b)

(* Convert a leaf to Bool in order to apply the function to the ROBDD *)
let leafToBool = function
  | One -> true
  | Zero -> false
  | _ -> raise (Invalid_argument "Fail to convert leaf to bool")

let boolToLeaf = function
  true -> One
  | false -> Zero

(** HASHTABLE PART *)
type 'a robdd_id = uid * 'a * uid
type op_id = uid * uid

(* This is the hashtable that contains the various bdd nodes *)
let context:((Type.t robdd_id,Type.t bdd) Hashtbl.t) = (Hashtbl.create 2047)

(* This table contains the function cache *)
let opContext:((op_id,Type.t bdd) Hashtbl.t) = (Hashtbl.create 2047)


(** Safe implementation of Hashtbl.find. Ocaml does not provide such function *)
let lookup elt ctx =
  if Hashtbl.mem ctx elt then
    Some (Hashtbl.find ctx elt)
  else
    None

let lookupUnsafe = Hashtbl.find context

(* Predicate function that checks that a ROBDD is a singleton, i.e, a simple
 * variable *)
let rec isSingleton = function
  Robdd (l,_,r,_) when ((l == One) || (l == Zero)) &&
    ((r == One) || (r == Zero)) -> true
  | Robddref (left,v,right,_) -> isSingleton (lookupUnsafe (left,v,right))
  | _ -> false

(* Generates a node only if necessary *)
let mkNode l v r =
  let (lid,rid) = (identifier l,identifier r) in
  match lookup (lid,v,rid) context with
    | Some id -> Robddref (lid,v,rid,(identifier id))
    | None ->
    if (eq l r) then l
    else
      begin
        let res = Robdd (l,v,r,uidMake ()) in
        Hashtbl.add context (lid,v,rid) res;
        res
      end

(* Variable generation function *)
let singleton v = mkNode Zero v One

(* Generic function applying unary boolean operations. Returns the new ROBDD *)
let rec unaryApply fn = function
  Robdd (l,v,r,_) -> mkNode (unaryApply fn l) v (unaryApply fn r)
  | Robddref (l,v,r,_) -> unaryApply fn (lookupUnsafe (l,v,r))
  | rest -> boolToLeaf (fn (leafToBool rest))

(* Generic function for binary operations on ROBDD *)
let rec apply fn left right =
match lookup (identifier left, identifier right) opContext with
| Some a -> a
| None   ->
  let res =
  match (left,right) with
    | (Robdd (l,v,r,_),Robdd (l',v',r',_))
      -> begin
          match compare v v' with
          | 0 -> mkNode (apply fn l l') v (apply fn r r')
          | n when n < 0 -> mkNode (apply fn l right) v (apply fn r right)
          | _ -> mkNode (apply fn left l') v' (apply fn left r')
        end
    | (Robddref (l,v,r,_), other) -> apply fn (lookupUnsafe (l,v,r)) other
    | (other, Robddref (l,v,r,_)) -> apply fn other (lookupUnsafe (l,v,r))
    | (Zero, Robdd (l,v,r,_)) -> mkNode (apply fn Zero l) v (apply fn Zero r)
    | (Robdd (l,v,r,_), Zero) -> mkNode (apply fn l Zero) v (apply fn r Zero)
    | (One, Robdd (l,v,r,_)) -> mkNode (apply fn One l) v (apply fn One r)
    | (Robdd (l,v,r,_), One) -> mkNode (apply fn l One) v (apply fn r One)
    | (l,r) -> boolToLeaf (fn (leafToBool l) (leafToBool r))
  in
  Hashtbl.add opContext (identifier left, identifier right) res;
  res

(* The implementations of ROBDD Primitives, the function cache is cleared
 * between operations *)
let neg     a = (Hashtbl.clear opContext ; unaryApply not a)
let (&&.) a b = (Hashtbl.clear opContext ; apply (&&) a b)
let (||.) a b = (Hashtbl.clear opContext ; apply (||) a b)
let (^.)  a b = (Hashtbl.clear opContext ; apply (<>) a b)
let (=>)  a b = (Hashtbl.clear opContext ; apply (fun a b -> (not a) || b) a b)
let (<=>) a b = (Hashtbl.clear opContext ; apply (==) a b)

(** Binary type, similar to Either in haskell. It describes the values taken
 * by the variables in a satisfiable path *)
type 'a value = True of 'a | False of 'a

(** Returns a satisfied expression in BDD *)
let rec getSat = function
  | Zero -> None
  | One -> Some []
  | Robdd (l,v,r,_) ->
      begin
        match getSat l with
        Some c -> Some (False v::c)
        | None ->
          begin
            match getSat r with
            Some c -> Some (True v::c)
            | None -> None
          end
      end
  | Robddref (l,v,r,_) -> getSat (lookupUnsafe (l,v,r))

(** Returns the list of satisfied expressions in a ROBDD *)
let rec getSatList = function
  Zero -> []
  | One -> [[]]
  | Robdd (l,v,r,_) ->
    (List.map (fun e -> (True v)::e) (getSatList l))
    @(List.map (fun e -> (False v)::e) (getSatList r))
  | Robddref (l,v,r,_) -> getSatList (lookupUnsafe (l,v,r))

let rec satCount = function
  | Zero -> 0
  | One -> 1
  | Robdd (l,v,r,_) -> (satCount l) + (satCount r)
  | Robddref (l,v,r,_) -> satCount (lookupUnsafe (l,v,r))

(** Sets a variable to a given Boolean value *)
let rec restrict var value = function
  | Robdd (l,v,r,_) ->
    if v == var then
      let dir = if value  then l else r in
      restrict var value dir
    else
      mkNode (restrict var value l) v (restrict var value r)
  | Robddref (l,v,r,_) -> restrict var value (lookupUnsafe (l,v,r))
  | n -> n

let exists' var bdd =
  (restrict var false bdd) ||. (restrict var true bdd)

(** Existential operation on a ROBDD *)
let rec exists var bdd = match var with
  | Robdd (l,v,r,_) when isSingleton var -> exists' v bdd
  | Robddref (l,v,r,_) -> exists var (lookupUnsafe (l,v,r))
  | _ -> raise (Invalid_argument "Exists: not a singleton")
end
