open Ppxlib

let mk_return ~loc x =
  [%expr Lwt.return_some [%e x]]

let mk_bind ~loc e f =
  [%expr
    let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
    Lwt.backtrace_bind
      (fun exn -> try Reraise.reraise exn with exn -> exn)
      [%e e]
      ((fun f -> function
          | None -> Lwt.return_none
          | Some x -> f x) [%e f])]

let () =
  Ppx_monad.register "olwt"
    ~mk_return ~mk_bind
