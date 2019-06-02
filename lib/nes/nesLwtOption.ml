type 'a t = 'a option Lwt.t

let bind : 'a t -> ('a -> 'b t) -> 'b t =
  fun x f ->
  match%lwt x with
  | None -> Lwt.return_none
  | Some x -> f x

let map : 'a t -> ('a -> 'b) -> 'b t =
  fun x f ->
  match%lwt x with
  | None -> Lwt.return_none
  | Some x -> Lwt.return_some (f x)

module Syntax = struct
  let (>>=|?) = bind
  let (<&>|?) = map
end
