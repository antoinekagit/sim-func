open CCFQueue

module type TYPE = sig
  type t
end

module type TYPE_ADD = sig
  include TYPE
  val add : t -> t -> t
end

module type TYPE_ADD_NIL = sig
  include TYPE_ADD
  val nil : t   (* must be neutral for add *)
end
    
module type MONOID = sig
  type t
  val nil : t
  val add : t -> t -> t
end

module type ORDERED = sig
  type t
  val compare : t -> t -> int
end

module type TYPE_POINT = sig
  type t
  val point : t
end
  
let get_or_else optA defaultA =
  match optA with
  | Some a -> a
  | None -> defaultA

module Int = struct
  type t = int
  let compare (a : int) (b : int) : int = (compare a b)
end


module Rand : sig
  type t
  val init : int array -> t
  val genInt : t -> int -> (int * t) (* arg is the exclusive bound *)
end = struct
  type t = Random.State.t
  let init = Random.State.make
  let genInt rand borne =
    let rand = Random.State.copy rand in
    let int = Random.State.int rand borne in
    (int, rand)
end

let (>@) a f = (f a)
let (>>) f g x = g (f x)

let flip f b a = (f a b)

module Liste = struct
  type 'a t = 'a list
  let return a = [a]
  let cons a la = a :: la
  let rec fold_left la res f =
    match la with
    | a :: tla -> fold_left tla (f a res) f
    | [] -> res
  let rev la = fold_left la [] cons
  let size la = fold_left la 0 (fun _a s -> s + 1)
  let rec has la target_a =
    match la with
    | a :: tla -> if a = target_a then true else (has tla target_a)
    | [] -> false
  let fmap f la = fold_left (rev la) [] (fun a -> cons (f a))
  let append la lb = fold_left (rev la) lb cons
  let join lla = fold_left (rev lla) [] append
    [@@@warning "-8"]
  let zip la lb =
    fold_left la ([], lb) (fun a (lab, b :: tlb) -> ((a, b) :: lab, tlb))
    >@ fst >@ rev
      [@@@warning "+8"]
  let take n la =
    let rec loop la n accA =
      if n <= 0 then Some (la, accA)
      else match la with
      | [] -> None
      | a :: tla -> loop tla (n - 1) (a :: accA) in
    loop la n []
  let take_atmax n la =
    let rec loop la n accA =
      if n <= 0 then accA
      else match la with
      | [] -> accA
      | a :: tla -> loop tla (n - 1) (a :: accA) in
    loop la n []
  let make n f =
    let rec loop i la =
      if i >= n then la
      else loop (i + 1) ((f i) :: la) in
    loop 0 []
end

module HMap (Key : ORDERED) : sig
  type 'a t
  type key = Key.t
  val nil : 'a t
  val has_key : key -> 'a t -> bool
  val set : key -> 'a -> 'a t -> 'a t
  val opt_get : key -> 'a t -> 'a option
  val exc_get : key -> 'a t -> 'a
  val fold : 'a t -> 'res -> (key -> 'a -> 'res -> 'res) -> 'res
  val size : 'a t -> int
  val toList : 'a t -> (key * 'a) list
  val keys : 'a t -> key list
  val values : 'a t -> 'a list
end = struct
  module M = Map.Make(Key)
  type 'a t = 'a M.t
  type key = Key.t
  let nil = M.empty
  let has_key = M.mem
  let set = M.add
  let exc_get = M.find
  let opt_get k ta = if M.mem k ta then Some (M.find k ta) else None
  let fold ta res f = M.fold f ta res
  let size = M.cardinal
  let toList = M.bindings
  let keys ta = Liste.fmap fst (toList ta)
  let values ta = Liste.fmap snd (toList ta)
end
  
let i2s = string_of_int
let s2i = int_of_string
                          
type ident = int
type amount = int

module Fifo : sig
  type 'a t
  val nil : 'a t
  val one : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t
  val (++) : 'a t -> 'a t -> 'a t
  val fold : 'res -> ('res -> 'a -> 'res) -> 'a t -> 'res
  val takeLast : int -> 'a t -> 'a list
end = struct
  type 'a t = 'a CCFQueue.t
  let nil = CCFQueue.empty
  let one = CCFQueue.singleton
  let concat = CCFQueue.append 
  let (++) = CCFQueue.append
  let fold res f fa = CCFQueue.fold f res fa
  let takeLast n fa = snd (CCFQueue.take_back_l n fa)
end

               
