type t =
  | EntityDoesNotExist of string * string
  | DependencyDoesNotExist of (string * string) * (string * string)
  | DependencyViolatesStatus of (string * string) * (string * string)
  | BadQuery of string
  | Unexpected
[@@deriving yojson]

exception Exn of t

let status = function
  | EntityDoesNotExist _ -> `Not_found
  | DependencyDoesNotExist _ -> `Internal_server_error
  | DependencyViolatesStatus _ -> `Internal_server_error
  | BadQuery _ -> `Bad_request
  | Unexpected -> `Internal_server_error

let dependency_does_not_exist ~source ~dependency =
  DependencyDoesNotExist(source, dependency)

let dependency_violates_status ~source ~dependency =
  DependencyViolatesStatus(source, dependency)

let fail e = raise (Exn e)
let lwt_fail e = Lwt.fail (Exn e)
