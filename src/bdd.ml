module BDD = 
  functor (Type :
    sig
      type t
      val compare : t -> t -> int
    end) ->
struct
(** ID PART *)
type uid = int

let uidMake =
let cur = ref 2 in
  (fun () -> (incr cur); !cur)

(** BDD PART *)
type 'a bdd = One | Zero
  | Robdd of ('a bdd) * 'a * ('a bdd) * uid
  | Robddref  of uid * 'a * uid * uid

let identifier = function
  | One -> 1
  | Zero -> 0
  | Robdd (_,_,_,id) -> id
  | Robddref (_,_,_,id) -> id

let eq a b = (identifier a) == (identifier b)

let leafToBool = function
  | One -> true
  | Zero -> false
  | _ -> raise (Invalid_argument "Fail to convert leaf to bool")

let boolToLeaf = function
  true -> One
  | false -> Zero

(** HASHTABLE PART *)
type 'a robdd_id = uid * 'a * uid

let context:((Type.t robdd_id,Type.t bdd) Hashtbl.t) = (Hashtbl.create 2047)


let lookup elt =
  if Hashtbl.mem context elt then
    Some (Hashtbl.find context elt)
  else
    None

let lookupUnsafe = Hashtbl.find context

let insert id bdd = Hashtbl.add context id bdd

let rec isSingleton = function
  Robdd (l,_,r,_) when ((l == One) || (l == Zero)) &&
    ((r == One) || (r == Zero)) -> true
  | Robddref (left,v,right,_) -> isSingleton (lookupUnsafe (left,v,right))
  | _ -> false

let mkNode l v r =
  let (lid,rid) = (identifier l,identifier r) in
  match lookup (lid,v,rid) with
    | Some id -> Robddref (lid,v,rid,(identifier id))
    | None ->
    if (eq l r) then l
    else
      begin
        let res = Robdd (l,v,r,uidMake ()) in
        Hashtbl.add context (lid,v,rid) res;
        res
      end

let singleton v = mkNode Zero v One

let rec unaryApply fn = function
  Robdd (l,v,r,_) -> mkNode (unaryApply fn l) v (unaryApply fn r)
  | Robddref (l,v,r,_) -> unaryApply fn (lookupUnsafe (l,v,r))
  | rest -> boolToLeaf (fn (leafToBool rest))


let rec apply fn left right =
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

let neg = unaryApply not
let (&&.) = apply (&&)
let (||.) = apply (||)
let (^.)  = apply (<>)
let (=>) a b = apply (fun a b -> (not a) || b) a b
let (<=>) = apply (==)

let rec getSat = function
  Zero -> None
  | One -> Some []
  | Robdd (l,v,r,_) ->
      begin
        match getSat l with
        Some c -> Some (v::c)
        | None ->
          begin
            match getSat r with
            Some c -> Some (v::c)
            | None -> None
          end
      end
  | Robddref (l,v,r,_) -> getSat (lookupUnsafe (l,v,r))

type 'a value = True of 'a | False of 'a

let rec getSatList = function
  Zero -> []
  | One -> [[]]
  | Robdd (l,v,r,_) ->
    (List.map (fun e -> (True v)::e) (getSatList l))
    @(List.map (fun e -> (False v)::e) (getSatList r))
  | Robddref (l,v,r,_) -> getSatList (lookupUnsafe (l,v,r))

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

let rec exists var bdd = match var with
  | Robdd (l,v,r,_) when isSingleton var -> exists' v bdd
  | Robddref (l,v,r,_) -> exists var (lookupUnsafe (l,v,r))
  | _ -> raise (Invalid_argument "Exists: not a singleton")
end
