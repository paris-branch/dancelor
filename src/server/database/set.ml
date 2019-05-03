open Nes
open Dancelor_common_model
module Unsafe = Dancelor_database_unsafe.Set

let get (slug : Set.t Slug.t) = Unsafe.get slug
let get_opt (slug : Set.t Slug.t) = Unsafe.get_opt slug
let get_all () = Unsafe.get_all ()

let save ?slug ~name ~kind ?status ~tunes () =
  ignore slug; ignore name; ignore kind; ignore status; ignore tunes;
  assert false

let get_programs_that_contain (slug : Set.t Slug.t) : Program.t Seq.t =
  Dancelor_database_unsafe.Program.get_all ()
  |> Seq.filter (Program.contains slug)

exception UsedInProgram of Program.t Slug.t

let delete (set : Set.t Slug.t) =
  match Seq.hd_opt (get_programs_that_contain set) with
  | None -> Unsafe.delete set
  | Some program -> raise (UsedInProgram (Program.slug program))
