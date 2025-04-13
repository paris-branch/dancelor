open Nes
open Madge
open ModelBuilder

type (_, _, _) t =
  | Get : ((User.t Slug.t -> 'w), 'w, User.t Entry.t) t

let to_string : type a w r. (a, w, r) t -> string = function
  | Get -> "Get"

type wrapped = W : ('a, 'r Lwt.t, 'r) t -> wrapped
let all = [W Get]

let route : type a w r. (a, w, r) t -> (a, w, r) route = function
  | Get -> variable (module SSlug(User)) @@ get (module Entry.J(User))
