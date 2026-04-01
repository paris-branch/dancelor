(* necessary for Lwt's backtrace bind *)
module Reraise = struct
  external reraise : exn -> 'a = "%reraise"
end

type 'a t = 'a option Lwt.t

let bind e f =
  (* FIXME: Since recent versions, [Lwt.backtrace_bind] wants the filename and
     the line number, presumably to track things better in backtraces. *)
  Lwt.backtrace_bind
    "NesOlwt FIXME"
    0
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    e
    (function
      | Some x -> f x
      | None -> Lwt.return_none
    )
