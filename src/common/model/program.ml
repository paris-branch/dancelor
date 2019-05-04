open Nes

type t =
  { slug : t Slug.t ;
    name : string ;
    date : Date.t ;
    status : Status.t ;
    sets : Set.t Slug.t list }
[@@deriving yojson]

let slug p = p.slug
let date p = p.date

let contains s p =
  List.mem s p.sets

let compare p1 p2 =
  (* Compare first by date *)
  let c = compare p1.date p2.date in
  if c = 0 then
    compare p1 p2
  else
    c
