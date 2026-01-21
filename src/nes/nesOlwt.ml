(* necessary for Lwt's backtrace bind *)
module Reraise = struct
  external reraise : exn -> 'a = "%reraise"
end

type 'a t = 'a option Lwt.t

let bind e f =
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    e
    (function
      | Some x -> f x
      | None -> Lwt.return_none
    )
