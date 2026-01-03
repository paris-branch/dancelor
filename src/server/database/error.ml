type t =
  | Dependency_does_not_exist of (string * string) * (string * string)
  | Entity_has_reverse_dependencies of unit
  | Storage_read_only
[@@deriving yojson, show {with_path = false}]

exception Exn of t

let status = function
  | Dependency_does_not_exist _ -> `Internal_server_error
  | Entity_has_reverse_dependencies _ -> `Bad_request
  | Storage_read_only -> `Service_unavailable

let dependency_does_not_exist ~source ~dependency =
  Dependency_does_not_exist (source, dependency)

let fail e = raise (Exn e)
let lwt_fail e = Lwt.fail (Exn e)
