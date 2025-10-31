(* necessary for Lwt's backtrace bind *)
module Reraise = struct
  external reraise : exn -> 'a = "%reraise"
end

type ('a, 'e) t = ('a, 'e) Result.t Lwt.t

let bind e f =
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    e
    (function
      | Ok x -> f x
      | Error y -> Lwt.return_error y
    )
