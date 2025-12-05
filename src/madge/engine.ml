open Nes

open Request
open Serialisation
open Route_internal

module Log = (val Logs.(src_log (Src.create "madge.engine")): Logs.LOG)

(* route -> request *)

exception IllegalBodyInGetRequest

let rec with_request
  : type a w r. string ->
  Query.t ->
  Query.t ->
  (a, w, r) Route.t ->
  ((module JSONABLE with type t = r) -> Request.t -> w) ->
  a
= fun path query body route return ->
  match route with
  | Return (meth, (module R)) ->
    (
      let uri = Uri.make ~path ~query: (Query.to_strings query) () in
      let body =
        if meth = GET then
          (
            if not (Query.is_empty body) then
              raise IllegalBodyInGetRequest;
            ""
          )
        else
          Yojson.Safe.to_string @@ `Assoc (Query.to_list body)
      in
      return (module R) (Request.make ~meth ~uri ~body)
    )
  | Literal (str, route) ->
    with_request (path ^ "/" ^ str) query body route return
  | Variable (prefix, (module R), suffix, route) ->
    fun x -> with_request (path ^ "/" ^ prefix ^ Uri.pct_encode (R.to_string x) ^ suffix) query body route return
  | Query (source, name, _, unproxy, (module R), route) ->
    (
      unproxy @@ function
        | None ->
          with_request path query body route return
        | Some x ->
          let (query, body) =
            match source with
            | Uri -> (Query.add name (R.to_yojson x) query, body)
            | Body -> (query, Query.add name (R.to_yojson x) body)
          in
          with_request path query body route return
    )

let with_request
  : type a w r. (a, w, r) Route.t ->
  ((module JSONABLE with type t = r) -> Request.t -> w) ->
  a
= fun route return ->
  with_request "" Query.empty Query.empty route return

let uri : type a r. (a, Uri.t, r) Route.t -> a = fun route ->
  with_request route (fun (module _) request -> Request.uri request)

(* request -> route *)

(* NOTE: The [controller] is in a thunk to avoid it being ran halfway as we find
   its last argument. It is actually run at the end when all is green. *)
let rec apply
  : type a w r z. (a, w, r) Route.t ->
  (unit -> a) ->
  Request.meth ->
  string list ->
  Query.t ->
  Query.t ->
  ((module JSONABLE with type t = r) -> (unit -> w) -> z) ->
  (unit -> z) option
= fun route controller meth path query body return ->
  match route with
  | Return (meth', (module R)) ->
    (
      Log.debug (fun m -> m "  Return (%s, <module R>)" (Request.meth_to_string meth'));
      if meth' = meth && path = [] && Query.is_empty query && Query.is_empty body then
        Some (fun () -> return (module R) controller)
      else
        None
    )
  | Literal (str, route) ->
    (
      Log.debug (fun m -> m "  Literal (\"%s\", <route>)" str);
      match path with
      | comp :: path when comp = str -> apply route controller meth path query body return
      | _ -> None
    )
  | Variable (prefix, (module R), suffix, route) ->
    (
      Log.debug (fun m ->
        m
          "  Variable (\"%s\", <module R>, \"%s\", <route>) [path = %a]"
          prefix
          suffix
          Format.(pp_print_list ~pp_sep: (fun fmt () -> fprintf fmt " ") pp_print_string)
          path
      );
      match path with
      | [] -> None
      | comp :: path ->
        Option.bind (String.remove_prefix ~needle: prefix comp) @@ fun comp ->
        Option.bind (String.remove_suffix ~needle: suffix comp) @@ fun comp ->
        Option.bind (R.of_string comp) @@ fun comp ->
        apply route (fun () -> controller () comp) meth path query body return
    )
  | Query (source, name, proxy, _, (module R), route) ->
    (
      Log.debug (fun m -> m "  Query (\"%s\", <proxy>, ???, <module R>, <route>)" name);
      let extract_and_parse =
        match (source, Query.extract name query, Query.extract name body) with
        | (Uri, None, _) ->
          Log.debug (fun m -> m "    Could not find query argument `%s`" name);
          Ok (None, query, body) (* absent: OK *)
        | (Body, _, None) ->
          Log.debug (fun m -> m "    Could not find body argument `%s`" name);
          Ok (None, query, body) (* absent: OK *)
        | (Uri, Some (value, query), _) ->
          (
            match R.of_yojson value with
            | Ok value -> Ok (Some value, query, body)
            | Error msg ->
              Log.debug (fun m -> m "    Found query argument `%s` but failed to unserialise it: %s" name msg);
              Error "unparseable" (* present but unparseable: error *)
          )
        | (Body, _, Some (value, body)) ->
          (
            match R.of_yojson value with
            | Ok value -> Ok (Some value, query, body)
            | Error msg ->
              Log.debug (fun m -> m "    Found body argument `%s` but failed to unserialise it: %s" name msg);
              Error "unparseable" (* present but unparseable: error *)
          )
      in
      match extract_and_parse with
      | Error _ -> None (* unparseable: the route does not match *)
      | Ok (maybe_value, query, body) ->
        match proxy maybe_value with
        | None -> None
        | Some f -> apply route (fun () -> f (controller ())) meth path query body return
    )

let apply
  : type a w r z. (a, w, r) Route.t ->
  (unit -> a) ->
  Request.t ->
  ((module JSONABLE with type t = r) -> (unit -> w) -> z) ->
  (unit -> z) option
= fun route controller request return ->
  Log.debug (fun m -> m "Madge.apply <route> <controller> <request> <return>");
  let path = List.filter ((<>) "") (String.split_on_char '/' (Uri.path @@ Request.uri request)) in
  Option.bind (Query.from_uri @@ Request.uri request) @@ fun uri_query ->
  apply route controller (Request.meth request) path uri_query (Query.from_body @@ Request.body request) return

let apply'
  : type a w r. (a, w, r) Route.t ->
  (unit -> a) ->
  Request.t ->
  (unit -> w) option
= fun route controller request ->
  apply route controller request (fun _ f -> f ())
