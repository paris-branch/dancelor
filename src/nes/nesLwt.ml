include Lwt

let compose f g = fun x ->
  bind (f x) g

let if_ b f = if%lwt b then f () else Lwt.return_unit
let if_' b f = if b then f () else Lwt.return_unit

(** Make a pool of promises such that adding a new one cancels the previous
    one. *)
let replaceable () =
  let current = ref @@ Lwt.fail (Failure "replaceable_promise") in
  fun promise ->
    Lwt.cancel !current;
    current := promise;
    promise

let l2 f a b =
  let%lwt a = a in
  let%lwt b = b in
  Lwt.return (f a b)
