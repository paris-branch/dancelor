
type t =
  { name : string ;
    kind : Kind.dance ;
    credit : Person.t list }

let create name kind credit =
  { name ; kind ; credit }
