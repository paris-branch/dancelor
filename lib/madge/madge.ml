include Serialisers

type 'a arg =
  { opt: bool ;
    key : string ;
    serialiser : 'a serialiser ;
    unserialiser : 'a unserialiser }

(* FIXME: check either through typing or dynamically that optional arguments
   are indeed used optionally. *)

let arg ~key ~serialiser ~unserialiser =
  { opt = false; key; serialiser; unserialiser }

let optarg ~key ~serialiser ~unserialiser =
  { opt = true; key; serialiser; unserialiser }

type 'a endpoint =
  { meth : Cohttp.Code.meth ;
    path : string ;
    serialiser : 'a serialiser ;
    unserialiser : 'a unserialiser }

let endpoint ~meth ~path ~serialiser ~unserialiser =
  { meth; path; serialiser; unserialiser }

type handler = query:(string * string list) list -> serialised Lwt.t

let handlers : (Cohttp.Code.meth * string, handler) Hashtbl.t = Hashtbl.create 8

type query = (string * string list) list ref

exception BadQuery of string
let bad_query s = raise (BadQuery s)

let get_opt_arg query arg =
  match List.assoc_opt arg.key !query with
  | None -> None
  | Some [x] ->
    (match arg.unserialiser (Yojson.Safe.from_string x) with
     | Ok x -> Some x
     | Error msg ->
       Format.ksprintf
         bad_query
         "could not unserialise %s: %s"
         arg.key msg)
  | Some [] ->
    Format.ksprintf
      bad_query
      "empty list for %s"
      arg.key
  | Some _ ->
    Format.ksprintf
      bad_query
      "more than one values for %s"
      arg.key

let get_arg query arg =
  match get_opt_arg query arg with
  | None ->
    Format.ksprintf
      bad_query
      "missing mandatory %s"
      arg.key
  | Some x -> x

(* FIXME: factorise exception raising *)
(* FIXME: catch exceptions from [from_string] *)

let register ~endpoint controller =
  Hashtbl.add handlers (endpoint.meth, endpoint.path) @@ fun ~query ->
  let query = ref query in
  let%lwt val_ = controller query in
  Lwt.return (endpoint.serialiser val_)

(* FIXME: check that all query is used; each argument exactly once *)

let add_arg query arg val_ =
  query := (arg.key, [Yojson.Safe.to_string (arg.serialiser val_)]) :: !query

let add_opt_arg query arg val_ =
  match val_ with
  | None -> ()
  | Some val_ -> query := (arg.key, [Yojson.Safe.to_string (arg.serialiser val_)]) :: !query

let call ~endpoint query_builder =
  let query = ref [] in
  query_builder query;
  let%lwt (response, body) =
    Cohttp_lwt_xhr.Client.call
      endpoint.meth
      (Uri.make ~path:endpoint.path ~query:!query ())
  in
  if Cohttp.(Code.(is_success (code_of_status (Response.status response)))) then
    (
      let%lwt body = Cohttp_lwt.Body.to_string body in
      let body = Yojson.Safe.from_string body in
      match endpoint.unserialiser body with
      | Ok x -> Lwt.return x
      | Error _ -> assert false (* FIXME *)
    )
  else
    assert false (* FIXME *)

(* FIXME: handle errors in request and unserialiser *)
