open Nes
open Madge

module Response = struct
  type status =
    Running | Succeeded | Failed
  [@@deriving yojson]

  type t = {
    status: status;
    stdout: string;
    stderr: string;
  }
  [@@deriving yojson]
end

type (_, _, _) t =
  | Status : (JobId.t -> 'w, 'w, Response.t) t
  | File : (JobId.t -> Entry.Slug.t -> 'w, 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Status -> literal "status" @@ variable (module JobId) @@ post (module Response)
    | File -> literal "file" @@ variable (module JobId) @@ variable (module Entry.Slug.S) @@ void ()
