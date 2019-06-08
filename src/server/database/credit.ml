open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe
module Log = (val Dancelor_server_logs.create "server.database.credit" : Logs.LOG)

let get (slug : Model.Credit.t Slug.t) = Unsafe.Credit.get slug
let get_all = Unsafe.Credit.get_all

let save = Unsafe.Credit.save

let initialise =
  Unsafe.Credit.initialise
  >=>| Unsafe.Credit.get_all
  >=>| Lwt_list.iter_s
    (fun credit ->
       let%lwt slug = Model.Credit.slug credit in
       Log.debug (fun m -> m "Checking consistency for %s" slug);
       let%lwt status = Model.Credit.status credit in
       Model.Credit.persons credit
       >>=| Lwt_list.iter_s (Unsafe.Person.check_status_ge ~status))
