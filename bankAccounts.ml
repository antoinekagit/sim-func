open Util
       
module Accs : sig
  type t
  type transfert_details = (ident * amount * ident)
  val nil : t
  val bankIdent : ident
  val try_get_amount : ident -> t -> (amount option)
  val get_amount : ident -> t -> amount
  val try_make_transfert : transfert_details -> t -> (t option)
  val exc_make_transfert : transfert_details -> t -> t
end = struct
  type transfert_details = (ident * amount * ident)
    
  module KeyIdent = struct
    type t = ident
    let compare (a : int) (b : int) : int = (compare a b)
  end
  module MapAccs = Map.Make(KeyIdent)

  let find_opt ident accs =
    if MapAccs.mem ident accs
    then Some (MapAccs.find ident accs)
    else None
                           
  type t = (amount MapAccs.t)

  let nil = MapAccs.empty
  let bankIdent = -1

  let try_get_amount ident accs = find_opt ident accs
  let get_amount ident accs =
    if MapAccs.mem ident accs
    then MapAccs.find ident accs
    else 0
    
  let try_make_transfert (source, amount, dest) accs =
    let sourceAmount = get_amount source accs in
    if sourceAmount >= amount || source = bankIdent
    then
      let destAmount = get_amount dest accs in
      let accs2 = MapAccs.add source (sourceAmount - amount) accs  in
      let accs3 = MapAccs.add dest   (destAmount + amount)   accs2 in
      Some accs3
    else None
      
  let exc_make_transfert (source, amount, dest) accs =
    let sourceAmount = get_amount source accs in
    if sourceAmount >= amount || source = bankIdent
    then
      let destAmount = get_amount dest accs in
      let accs2 = MapAccs.add source (sourceAmount - amount) accs  in
      let accs3 = MapAccs.add dest   (destAmount + amount)   accs2 in
      accs3
    else failwith "exception transfert : source has insufficient amount"
end
