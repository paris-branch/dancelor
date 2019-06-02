open Madge_common

type handler = query:(string * string list) list -> serialised Lwt.t

let handlers : (Cohttp.Code.meth * string, handler) Hashtbl.t = Hashtbl.create 8

let get_opt_arg query arg =
  match List.assoc_opt (arg_key arg) !query with
  | None -> None
  | Some [x] ->
    (match arg_unserialiser arg (Yojson.Safe.from_string x) with
     | Ok x -> Some x
     | Error msg ->
       Format.ksprintf
         bad_query
         "could not unserialise %s: %s"
         (arg_key arg) msg)
  | Some [] ->
    Format.ksprintf
      bad_query
      "empty list for %s"
      (arg_key arg)
  | Some _ ->
    Format.ksprintf
      bad_query
      "more than one values for %s"
      (arg_key arg)

let get_arg query arg =
  match get_opt_arg query arg with
  | None ->
    Format.ksprintf
      bad_query
      "missing mandatory %s"
      (arg_key arg)
  | Some x -> x

(* FIXME: factorise exception raising *)
(* FIXME: catch exceptions from [from_string] *)

let register ~endpoint controller =
  Hashtbl.add handlers (endpoint_meth endpoint, endpoint_path endpoint) @@ fun ~query ->
  let query = ref query in
  let%lwt val_ = controller query in
  Lwt.return (endpoint_serialiser endpoint val_)

(* FIXME: check that all query is used; each argument exactly once *)

let handle meth path query =
  let l = String.length !prefix in
  if String.length path >= l && String.sub path 0 l = !prefix then
    (
      let path = String.sub path l (String.length path - l) in
      match Hashtbl.find_opt handlers (meth, path) with
      | None ->
        Lwt.return_none
      | Some handler ->
        let%lwt serialised = handler ~query in
        let%lwt res = Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:(Yojson.Safe.to_string serialised) () in
        Lwt.return_some res
    )
  else
    Lwt.return_none
