open Nes
module Model = Dancelor_common_model
module Unsafe = Dancelor_server_database_unsafe

let get (slug : Model.Dance.t Slug.t) = Unsafe.Dance.get slug
let get_all = Unsafe.Dance.get_all

let initialise =
  Unsafe.Dance.initialise
  >=>| Unsafe.Dance.get_all
  >=>| Lwt_list.iter_s
    (fun dance ->
       let%lwt status = Model.Dance.status dance in
       Model.Dance.deviser dance
       >>=| Option.ifsome_lwt (Unsafe.Credit.check_status_ge ~status))
