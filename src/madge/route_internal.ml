open Serialisation

(** The type arguments are (1) the function type corresponding to the
    route, often named ['a], (2) the return value of that function
    type, often named ['w] for “omega”, and (3) the return value from
    the route, often named ['r].

    In [Query_or_body], [proxy]/[unproxy] allow manipulating a type of
    interest (['y]) via a type that we know how to serialise
    (['x]). The constructors [`Present]/[`Absent] refer to the
    presence in the request. The constructors [`Match]/[`Dont_match]
    refer to whether the route should be considered to match. It is
    expected that [proxy (unproxy y) = `Match y]. *)
type (_, _, _) t =
  | Return :
    {
      meth: Request.meth;
      serialiser: (module JSONABLE with type t = 'r);
    } ->
      ('w, 'w, 'r) t
  | Literal :
    {
      str: string;
      rest: ('a, 'w, 'r) t;
    } ->
      ('a, 'w, 'r) t
  | Variable :
    {
      prefix: string;
      serialiser: (module STRINGABLE with type t = 'x);
      suffix: string;
      rest: ('a, 'w, 'r) t;
    } ->
      (('x -> 'a), 'w, 'r) t
  | Query_or_body :
    {
      kind: [
        | `Query_string of (module STRINGABLE with type t = 'x)
        | `Query_json of (module JSONABLE with type t = 'x)
        | `Body of (module JSONABLE with type t = 'x)
      ];
      name: string;
      proxy: ([`Present of 'x | `Absent] -> [`Match of 'y | `Dont_match]);
      unproxy: ('y -> [`Present of 'x | `Absent]);
      rest: ('a, 'w, 'r) t;
    } ->
      (('y -> 'a), 'w, 'r) t
