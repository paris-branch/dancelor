
type t =
  { name : string }

let create name =
  { name }

let name p = p.name
