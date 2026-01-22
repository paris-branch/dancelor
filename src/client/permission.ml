open Nes

(** {2 Common tests and assertions} *)

include Common.Permission_builder
include Make(Model.User)

(** {3 Tests} *)

let can can = fun () ->
  can <$> Environment.user

let can_get_public entry = can (flip can_get_public entry) ()

let can_create_public = can can_create_public

let can_update_public entry = can (flip can_update_public entry) ()

let can_delete_public entry = can (flip can_delete_public entry) ()

let can_get_private entry = can (flip can_get_private entry) ()

let can_create_private = can can_create_private

let can_update_private entry = can (flip can_update_private entry) ()

let can_delete_private entry = can (flip can_delete_private entry) ()

(** {2 Ad-hoc tests and assertions} *)

let can_administrate () =
  match%lwt Environment.user with
  | None -> lwt_false
  | Some user -> lwt @@ Model.User.is_administrator' user
