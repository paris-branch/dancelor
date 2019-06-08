open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe

let get (slug : Model.Tune.t Slug.t) = Unsafe.Tune.get slug
let get_all = Unsafe.Tune.get_all

let read_content tune =
  Unsafe.Tune.read_separated_file tune "content.ly"

let write_content tune content =
  Unsafe.Tune.write_separated_file tune "content.ly" content

let initialise =
  Unsafe.Tune.initialise
  >=>| Unsafe.Tune.get_all
  >=>| Lwt_list.iter_s
    (fun tune ->
       let%lwt status = Model.Tune.status tune in
       Model.Tune.group tune
       >>=| Unsafe.TuneGroup.check_status_ge ~status >>=| fun () ->
       Model.Tune.arranger tune
       >>=| Option.ifsome_lwt (Unsafe.Credit.check_status_ge ~status) >>=| fun () ->
       Model.Tune.sources tune
       >>=| Lwt_list.iter_s (Unsafe.Source.check_status_ge ~status) >>=| fun () ->
       Model.Tune.dances tune
       >>=| Lwt_list.iter_s (Unsafe.Dance.check_status_ge ~status))
