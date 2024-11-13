open Madge_common

type handler = body: (string * Yojson.Safe.t) list -> serialised Lwt.t

let handlers : (Cohttp.Code.meth * string, handler) Hashtbl.t = Hashtbl.create 8

type get_arg = {a: 'a. ('a, mandatory) arg -> 'a}
type get_opt_arg = {o: 'a. ('a, optional) arg -> 'a option}

let get_opt_arg query : get_opt_arg =
  let o arg =
    match List.assoc_opt (arg_key arg) !query with
    | None -> None
    | Some x ->
      (
        match arg_unserialiser arg x with
        | Ok x -> Some x
        | Error msg ->
          Format.ksprintf
            bad_query
            "could not unserialise %s: %s"
            (arg_key arg)
            msg
      )
  in
  {o}

let get_arg query : get_arg =
  let a arg =
    match List.assoc_opt (arg_key arg) !query with
    | None ->
      Format.ksprintf
        bad_query
        "missing mandatory %s"
        (arg_key arg)
    | Some x ->
      (
        match arg_unserialiser arg x with
        | Ok x -> x
        | Error msg ->
          Format.ksprintf
            bad_query
            "could not unserialise %s: %s"
            (arg_key arg)
            msg
      )
  in
  {a}

(* FIXME: factorise exception raising *)
(* FIXME: catch exceptions from [from_string] *)

let register ~endpoint controller =
  Hashtbl.add handlers (endpoint_meth endpoint, endpoint_path endpoint) @@ fun ~body ->
  let body = ref body in
  let get_opt_arg = get_opt_arg body in
  let get_arg = get_arg body in
  let%lwt val_ = controller get_arg get_opt_arg in
  Lwt.return (endpoint_serialiser endpoint val_)

(* FIXME: check that all query is used; each argument exactly once *)

let handle meth path body =
  let l = String.length !prefix in
  if String.length path >= l && String.sub path 0 l = !prefix then
    (
      let path = String.sub path l (String.length path - l) in
      match Hashtbl.find_opt handlers (meth, path) with
      | None ->
        Lwt.return_none
      | Some handler ->
        let%lwt serialised = handler ~body in
        let%lwt res = Cohttp_lwt_unix.Server.respond_string ~status: `OK ~body: (Yojson.Safe.to_string serialised) () in
        Lwt.return_some res
    )
  else
    Lwt.return_none
