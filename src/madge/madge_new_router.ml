open Madge_common

(** {2 Routes} *)

(* type ('result, 'server) route = *)
(*   | Return : *)
(*       ( *)
(*         (Cohttp.Response.t * Cohttp_lwt.Body.t), *)
(*         unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t *)
(*       ) route *)
(*   | ReturnValue : *)
(*       ( *)
(*         ('a -> Yojson.Safe.t) *)
(*         * (Yojson.Safe.t -> 'a option) *)
(*       ) *)
(*       -> ('a, unit -> 'a Lwt.t) route *)
(*   | Query : *)
(*       ( *)
(*         string *)
(*         * ('a -> Yojson.Safe.t) *)
(*         * (Yojson.Safe.t -> 'a option) *)
(*         * ('result, 'server) route *)
(*       ) *)
(*       -> ('result, 'a option -> 'server) route *)
(*   | Literal : *)
(*       ( *)
(*         string *)
(*         * ('result, 'server) route *)
(*       ) *)
(*       -> ('result, 'server) route *)
(*   | Capture : *)
(*       ( *)
(*         string option *)
(*         * (string -> 'a option) *)
(*         * ('result, 'server) route *)
(*       ) *)
(*       -> ('result, 'a -> 'server) route *)

type 'server route =
  | Return :
      (
        unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t)
      ) route
  | ReturnValue :
      (
        ('a -> Yojson.Safe.t)
        * (Yojson.Safe.t -> 'a option)
      )
      -> (unit -> 'a) route
  | Query :
      (
        string
        * ('a -> Yojson.Safe.t)
        * (Yojson.Safe.t -> 'a option)
        * 'server route
      )
      -> ('a option -> 'server) route
  | Literal :
      (
        string
        * 'server route
      )
      -> 'server route
  | Capture :
      (
        string option
        * (string -> 'a option)
        * 'server route
      )
      -> ('a -> 'server) route

(** {2 Building routes} *)

let return = Return

let returnValue
      (type s)
      (module S : SERIALISABLE with type t = s)
  =
  ReturnValue (S.to_yojson, (fun x -> Result.to_option (S.of_yojson x)))

let query
      (type s)
      (key : string)
      (module S : SERIALISABLE with type t = s)
      route
  =
  Query (key, S.to_yojson, (fun x -> Result.to_option (S.of_yojson x)), route)

let literal
      (str : string)
      route
  =
  Literal (str, route)

let capture ?ext unserialise route =
  Capture (ext, unserialise, route)

(** {2 Using routes} *)

(* let rec apply *)
(*           (type result) *)
(*           (type server) *)
(*           (route : (result, server) route) *)
(*           (callback : server) *)
(*           (path : string list) *)
(*           (query : Madge_query.t) *)
(*         : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t option *)
(*   = *)

let rec apply
          (type server)
          (route : server route)
          (callback : server)
          (path : string list)
          (query : Madge_query.t)
        : (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t option
  =
  match (route, path) with
  (* | (Return, []) -> *)
  (*    Option.some @@ callback () *)
  (* | (ReturnValue (to_yojson, _), []) -> *)
  (*    Some ( *)
  (*        let%lwt body = callback () in *)
  (*        let body = Yojson.Safe.to_string @@ to_yojson body in *)
  (*        (\* FIXME: JSON content-type? *\) *)
  (*        Cohttp_lwt_unix.Server.respond_string ~status: `OK ~body () *)
  (*      ) *)
  (* | (Query (key, _, of_yojson, route), _) -> *)
  (*    ( *)
  (*      match Madge_query.extract key query with *)
  (*      | None -> apply route (callback None) path query *)
  (*      | Some (value, query) -> *)
  (*          Option.bind (of_yojson value) @@ fun value -> *)
  (*                                           apply route (callback (Some value)) path query *)
  (*    ) *)
  | (Literal (str, route), comp :: path) when str = comp ->
     apply route callback path query
  (* | (Capture (ext, of_string, route), comp :: path) -> *)
  (*    ( *)
  (*      Option.bind (Filename.remove_extension ext comp) @@ fun comp -> *)
  (*                                                          Option.bind (of_string comp) @@ fun comp -> *)
  (*                                                                                          apply route (callback comp) path query *)
  (*    ) *)
  | _ -> assert false

let apply
      (route : ('result, 'server) route)
      (callback : 'server)
      (uri : Uri.t)
    : unit Lwt.t option
  =
  let path = String.split_on_char '/' @@ Uri.path uri in
  let query = Uri.query uri in
  apply callback path query route
