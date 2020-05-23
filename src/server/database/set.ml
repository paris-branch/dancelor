open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "database.set" : Logs.LOG)

let get (slug : Model.Set.t Slug.t) = Unsafe.Set.get slug
let get_all = Unsafe.Set.get_all
let save = Unsafe.Set.save

let get_programs_that_contain (slug : Model.Set.t Slug.t) : Model.Program.t list Lwt.t =
  let%lwt all = Dancelor_server_database_unsafe.Program.get_all () in
  Lwt.return (List.filter (Model.Program.contains slug) all)

exception UsedInProgram of Model.Program.t Slug.t

let delete (set : Model.Set.t Slug.t) =
  let%lwt all = get_programs_that_contain set in
  match all with
  | [] ->
    Unsafe.Set.delete set
  | program :: _ ->
    let%lwt slug = Model.Program.slug program in
    Lwt.fail (UsedInProgram slug)
