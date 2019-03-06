open Dancelor_common
open Dancelor_model
module Unsafe = Dancelor_database_unsafe.Set

let get (slug : Set.t Slug.t) = Unsafe.get slug
let get_opt (slug : Set.t Slug.t) = Unsafe.get_opt slug
let get_all () = Unsafe.get_all ()

let save ?slug ~name ~kind ?status ~tunes () =
  ignore slug; ignore name; ignore kind; ignore status; ignore tunes;
  assert false

let get_programs_that_contain (slug : Set.t Slug.t) : Program.t list =
  Dancelor_database_unsafe.Program.get_all ()
  |> List.filter (Program.contains slug) (*FIXME: seq*)

exception UsedInProgram of Program.t Slug.t

let delete (set : Set.t Slug.t) =
  match get_programs_that_contain set with
  | program :: _ -> raise (UsedInProgram (Program.slug program))
  | [] -> Unsafe.delete set
