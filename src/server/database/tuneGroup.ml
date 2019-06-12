open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "server.database.tune-group" : Logs.LOG)

let get (slug : Model.TuneGroup.t Slug.t) = Unsafe.TuneGroup.get slug
let get_all = Unsafe.TuneGroup.get_all

let initialise =
  Unsafe.TuneGroup.initialise
  >=>| Unsafe.TuneGroup.get_all
  >=>| Lwt_list.iter_s
    (fun tune_group ->
       let%lwt slug = Model.TuneGroup.slug tune_group in
       Log.debug (fun m -> m "Checking consistency for %s" slug);
       let%lwt status = Model.TuneGroup.status tune_group in
       Model.TuneGroup.author tune_group
       >>=| Option.ifsome_lwt (Unsafe.Credit.check_status_ge ~status) >>=| fun () ->
       Model.TuneGroup.dances tune_group
       >>=| Lwt_list.iter_s (Unsafe.Dance.check_status_ge ~status))
