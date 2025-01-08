(** {1 Madge}

    Simple GADT-based routing. *)

open Nes

module Request = struct
  type t = {
    meth: Cohttp.Code.meth;
    uri: Uri.t;
    body: string;
  }

  let make ?(body = "") meth uri = {body; meth; uri}
end

module PathComponent = struct
  type 'a t = {
    to_string: ('a -> string);
    of_string: (string -> 'a option);
  }

  let void : Void.t t = {to_string = Void.f; of_string = Fun.const None}
  let unit = {to_string = (fun () -> ""); of_string = (function "" -> Some () | _ -> None)}
  let string = {to_string = Fun.id; of_string = Option.some}
  let int = {to_string = string_of_int; of_string = int_of_string_opt}
  let bool = {to_string = Format.sprintf "%b"; of_string = (fun s -> Scanf.sscanf_opt s "%b" Fun.id)}
end

module type JSON_SERIALISABLE = sig
  type t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

module Route = struct
  (** Abstract type of a route. The type arguments are (1) the function type
      corresponding to the route, (2) the return value of that function type, (3)
      the return value from the route. *)
  type (_, _, _) t =
    | Return : (module JSON_SERIALISABLE with type t = 'r) -> ('w, 'w, 'r) t
    | Literal : string * ('a, 'w, 'r) t -> ('a, 'w, 'r) t
    | Variable : 'a PathComponent.t * ('b, 'w, 'r) t -> (('a -> 'b), 'w, 'r) t

  let return rt = Return rt
  let literal str route = Literal (str, route)
  let variable rt route = Variable (rt, route)
end
