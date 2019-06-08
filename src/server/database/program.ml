open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe

let get (slug : Model.Program.t Slug.t) = Unsafe.Program.get slug
let get_all = Unsafe.Program.get_all

let initialise =
  Unsafe.Program.initialise
  >=>| Unsafe.Program.get_all
  >=>| Lwt_list.iter_s
    (fun program ->
       let%lwt status = Model.Program.status program in
       Model.Program.sets program
       >>=| Lwt_list.iter_s (Unsafe.Set.check_status_ge ~status))
