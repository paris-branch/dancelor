include Lwt

let compose f g = fun x ->
  bind (f x) g

(** [bind_return x f] binds [x] to [f], expecting a [unit] result, and then
    returns [x]. *)
let bind_return x f =
  x >>= fun x ->
  f x >>= fun () ->
  return x

let if_ b f = if%lwt b then f () else Lwt.return_unit
let if_' b f = if b then f () else Lwt.return_unit

module Syntax = struct
  let (>>=|) = bind
  let (>=>|) = compose
  let (>|=|) p f = map f p
end
