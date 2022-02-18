open Ppxlib

let mk_return ~loc x =
  [%expr Lwt.return (Ok [%e x])]

let mk_bind ~loc e f =
  [%expr
    let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
    Lwt.backtrace_bind
      (fun exn -> try Reraise.reraise exn with exn -> exn)
      [%e e]
      ((fun f -> function
          | Ok x -> f x
          | Error y -> Lwt.return (Error y)) [%e f])]

let mk_fail ~loc y =
  [%expr Lwt.return (Error [%e y])]

let mk_catch ~loc e f =
  [%expr
    let module Reraise = struct external reraise : exn -> 'a = "%reraise" end in
    Lwt.backtrace_bind
      (fun exn -> try Reraise.reraise exn with exn -> exn)
      [%e e]
      ((fun f -> function
          | Ok x -> Lwt.return (Ok x)
          | Error y -> f y) [%e f])]

let () =
  Ppx_monad.register "rlwt"
    ~mk_return ~mk_bind
    ~mk_fail ~mk_catch
