(* necessary for Lwt's backtrace bind *)
module Reraise = struct
  external reraise : exn -> 'a = "%reraise"
end

type 'a t = 'a Option.t Lwt.t

let return x = Lwt.return_some x
let fail () = Lwt.return_none

let bind e f =
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    e
    (
      function
      | Some x -> f x
      | None -> Lwt.return_none
    )

let catch e f =
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    e
    (
      function
      | Some x -> Lwt.return_some x
      | None -> f ()
    )
