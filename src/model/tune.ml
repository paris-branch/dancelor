
type t =
  { name : string ;
    kind : Kind.tune ;
    credit : Person.t list }

let create name kind credit =
  { name ; kind ; credit }
