open Nes

type t = [
  | `EntityDoesNotExist of string * string
  | `DependencyDoesNotExist of (string * string) * (string * string)
  | `DependencyViolatesStatus of (string * string) * (string * string)
  | `StorageReadOnly
  | `BadQuery of string
  | `Unexpected
]
[@@deriving yojson]

let status = function
  | `EntityDoesNotExist _ -> `Not_found
  | `DependencyDoesNotExist _ -> `Internal_server_error
  | `DependencyViolatesStatus _ -> `Internal_server_error
  | `StorageReadOnly -> `Service_unavailable
  | `BadQuery _ -> `Bad_request
  | `Unexpected -> `Internal_server_error

let entity_does_not_exist entity =
  Rlwt.fail (`EntityDoesNotExist entity)

let dependency_does_not_exist ~source ~dependency =
  Rlwt.fail (`DependencyDoesNotExist(source, dependency))

let dependency_violates_status ~source ~dependency =
  Rlwt.fail (`DependencyViolatesStatus(source, dependency))

let storage_read_only () =
  Rlwt.fail `StorageReadOnly
