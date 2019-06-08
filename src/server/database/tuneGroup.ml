open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe

let get (slug : Model.TuneGroup.t Slug.t) = Unsafe.TuneGroup.get slug
let get_all = Unsafe.TuneGroup.get_all

let initialise =
  Unsafe.TuneGroup.initialise
  >=>| Unsafe.TuneGroup.get_all
  >=>| Lwt_list.iter_s
    (fun tune_group ->
       let%lwt status = Model.TuneGroup.status tune_group in
       Model.TuneGroup.author tune_group
       >>=| Option.ifsome_lwt (Unsafe.Credit.check_status_ge ~status))
