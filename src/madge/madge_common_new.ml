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
    | Query : string * (module JSON_SERIALISABLE with type t = 'a) * ('b, 'w, 'r) t -> (('a -> 'b), 'w, 'r) t
    | QueryOpt : string * (module JSON_SERIALISABLE with type t = 'a) * ('b, 'w, 'r) t -> (('a option -> 'b), 'w, 'r) t

  let return rt = Return rt
  let literal str route = Literal (str, route)
  let variable rt route = Variable (rt, route)
  let query name rt route = Query (name, rt, route)
  let query_opt name rt route = QueryOpt (name, rt, route)

  let rec process
    : type a w r. Buffer.t ->
      Madge_query.t ->
      (a, w, r) t ->
      ((module JSON_SERIALISABLE with type t = r) -> Uri.t -> w) ->
      a
    = fun path query route return ->
      match route with
      | Return(module R) ->
        let uri = Uri.make ~path: (Buffer.contents path) ~query: (Madge_query.to_strings query) () in
        return (module R) uri
      | Literal (str, route) ->
        Buffer.add_char path '/';
        Buffer.add_string path str;
        process path query route return
      | Variable (rt, route) ->
        fun x ->
          Buffer.add_char path '/';
          (* FIXME: url encode *)
          Buffer.add_string path (rt.to_string x);
          process path query route return
      | Query (name, (module R), route) ->
        fun x ->
          let query = Madge_query.add name (R.to_yojson x) query in
          process path query route return
      | QueryOpt (name, (module R), route) ->
        function
        | None ->
          process path query route return
        | Some x ->
          let query = Madge_query.add name (R.to_yojson x) query in
          process path query route return

  let process
    : type a w r. (a, w, r) t ->
      ((module JSON_SERIALISABLE with type t = r) -> Uri.t -> w) ->
      a
    = fun route return ->
      process (Buffer.create 8) Madge_query.empty route return
end

let uri : type a r. (a, Uri.t, r) Route.t -> a = fun route ->
  Route.process route (fun (module _) uri -> uri)
