open Serialisation

type query_source = Uri | Body

(* NOTE: The type arguments are (1) the function type corresponding to the
   route, (2) the return value of that function type, (3) the return value from
   the route. *)
type (_, _, _) t =
  | Return : Request.meth * (module JSONABLE with type t = 'r) -> ('w, 'w, 'r) t
  | Literal : string * ('a, 'w, 'r) t -> ('a, 'w, 'r) t
  | Variable : string * (module STRINGABLE with type t = 'a) * string * ('b, 'w, 'r) t -> (('a -> 'b), 'w, 'r) t
  | Query :
    query_source
    * string
    *
      ('b option -> (('c -> 'a) -> 'a) option) (* proxy *)
    *
      (('b option -> 'a) -> ('c -> 'a)) (* unproxy *)
    *
      (module JSONABLE with type t = 'b)
    * ('a, 'w, 'r) t ->
      (('c -> 'a), 'w, 'r) t
