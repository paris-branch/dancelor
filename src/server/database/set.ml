open Nes
open Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe.Set

let get (slug : Set.t Slug.t) = Unsafe.get slug
let get_all () = Unsafe.get_all ()

let save ?slug ~name ~kind ?status ~tunes () =
  ignore slug; ignore name; ignore kind; ignore status; ignore tunes;
  assert false

let get_programs_that_contain (slug : Set.t Slug.t) : Program.t list Lwt.t =
  let%lwt all = Dancelor_server_database_unsafe.Program.get_all () in
  Lwt.return (List.filter (Program.contains slug) all)

exception UsedInProgram of Program.t Slug.t

let delete (set : Set.t Slug.t) =
  let%lwt all = get_programs_that_contain set in
  match all with
  | [] ->
    Unsafe.delete set
  | program :: _ ->
    let%lwt slug = Program.slug program in
    Lwt.fail (UsedInProgram slug)
