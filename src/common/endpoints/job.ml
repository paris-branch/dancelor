open Nes
open Madge

module Registration = struct
  type t =
    | AlreadySucceeded of JobId.t (** for when the job is cached and has already succeeded; avoids an extra call to check the status *)
    | Registered of JobId.t (** the job has been registered (whether it happened right now or already before) *)
  [@@deriving yojson]
end

module Status = struct
  type t =
    | Pending (** the server knows of the job *)
    | Running of string list (** the server is running the job; carries the logs *)
    | Failed of string list (** the job has failed; carries the logs *)
    | Succeeded (** the job has succeeded *)
  [@@deriving yojson]
end

type (_, _, _) t =
  | Status : (JobId.t -> 'w, 'w, Status.t) t
  | File : (JobId.t -> Entry.Slug.t -> 'w, 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Status -> variable (module JobId) @@ post (module Status)
    | File -> variable (module JobId) @@ variable (module Entry.Slug.S) @@ void ()
