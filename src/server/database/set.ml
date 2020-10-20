open Nes
module Model = Dancelor_common_model

include Tables.Set

let get slug = get slug
let get_all () = get_all ()

let get_programs_that_contain (slug : Model.Set.t Slug.t) : Model.Program.t list Lwt.t =
  let%lwt all = Tables.Program.get_all () in
  Lwt.return (List.filter (Model.Program.contains_set slug) all)

exception UsedInProgram of Model.Program.t Slug.t

let delete (set : Model.Set.t Slug.t) =
  let%lwt all = get_programs_that_contain set in
  match all with
  | [] ->
    delete set
  | program :: _ ->
    let%lwt slug = Model.Program.slug program in
    Lwt.fail (UsedInProgram slug)
