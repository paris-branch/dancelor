include Lwt

let compose f g = fun x ->
  bind (f x) g

let if_ b f = if%lwt b then f () else Lwt.return_unit
let if_' b f = if b then f () else Lwt.return_unit
