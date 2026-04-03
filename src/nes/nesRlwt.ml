(* necessary for Lwt's backtrace bind *)
module Reraise = struct
  external reraise : exn -> 'a = "%reraise"
end

type ('a, 'e) t = ('a, 'e) Result.t Lwt.t

let bind e f =
  (* FIXME: Since recent versions, [Lwt.backtrace_bind] wants the filename and
     the line number, presumably to track things better in backtraces. *)
  Lwt.backtrace_bind
    "NesRlwt FIXME"
    0
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    e
    (function
      | Ok x -> f x
      | Error y -> Lwt.return_error y
    )
