open Nes
open Dancelor_common

module Log = (val Logs.src_log @@ Logs.Src.create "server.permission": Logs.LOG)

(** {2 Common tests and assertions} *)

include Permission_builder
include Make(Model.User)

(** {3 Tests} *)

let can can = fun env ->
  let%lwt user = Environment.user env in
  lwt (can user <> None)

let can_get_public env entry = can (flip can_get_public entry % Option.map Database.User.entry_to_common) env

let can_create_public env = can (can_create_public % Option.map Database.User.entry_to_common) env

let can_update_public env entry = can (flip can_update_public entry % Option.map Database.User.entry_to_common) env

let can_delete_public env entry = can (flip can_delete_public entry % Option.map Database.User.entry_to_common) env

let can_get_private env entry = can (flip can_get_private entry % Option.map Database.User.entry_to_common) env

let can_create_private env = can (can_create_private % Option.map Database.User.entry_to_common) env

let can_update_private env entry = can (flip can_update_private entry % Option.map Database.User.entry_to_common) env

let can_delete_private env entry = can (flip can_delete_private entry % Option.map Database.User.entry_to_common) env

(** {3 Assertions} *)

let assert_ ~access_type ?(shortcut_forbidden = Madge_server.shortcut_forbidden) ~forbidden_message can = fun env ->
  if%lwt can env then
    (
      (* FIXME: print reason *)
      Log.debug (fun m -> m "Granting %s access to %a." access_type Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing %s access to %a." access_type Environment.pp env);
      shortcut_forbidden forbidden_message
    )

let assert_can_get_public env entry =
  assert_
    (flip can_get_public entry)
    env
    ~access_type: "public get"
    ~shortcut_forbidden: Madge_server.shortcut_not_found
    ~forbidden_message: "This entry does not exist, or you do not have access to it."

(** The rejection of {!assert_can_get_public}. This may be used by external code
    to behave in the exact same way and avoid leaking information. *)
let reject_can_get () =
  Madge_server.shortcut_not_found "This entry does not exist, or you do not have access to it."

let assert_can_create_public env =
  assert_
    can_create_public
    env
    ~access_type: "public create"
    ~forbidden_message: "You do not have permission to create this entry"

let assert_can_update_public env entry =
  assert_
    (flip can_update_public entry)
    env
    ~access_type: "public update"
    ~forbidden_message: "You do not have permission to update this entry"

let assert_can_delete_public env entry =
  assert_
    (flip can_delete_public entry)
    env
    ~access_type: "public deletion"
    ~forbidden_message: "You do not have permission to delete this entry"

let assert_can_get_private env entry =
  assert_
    (flip can_get_private entry)
    env
    ~access_type: "private get"
    ~shortcut_forbidden: Madge_server.shortcut_not_found
    ~forbidden_message: "This entry does not exist, or you do not have access to it."

let assert_can_create_private env =
  assert_
    can_create_private
    env
    ~access_type: "private create"
    ~forbidden_message: "You do not have permission to create this entry"

let assert_can_update_private env entry =
  assert_
    (flip can_update_private entry)
    env
    ~access_type: "private update"
    ~forbidden_message: "You do not have permission to update this entry"

let assert_can_delete_private env entry =
  assert_
    (flip can_delete_private entry)
    env
    ~access_type: "private delete"
    ~forbidden_message: "You do not have permission to delete this entry"

(** {2 Ad-hoc tests and assertions} *)

let is_connected env =
  let%lwt user = Environment.user env in
  lwt (user <> None)

let assert_is_connected env =
  if%lwt is_connected env then
    (
      Log.debug (fun m -> m "Granting access to %a." Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing access to %a." Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission."
    )

let can_administrate env =
  let%lwt user = Option.map Database.User.entry_to_common <$> Environment.user env in
  lwt @@ Option.fold user ~none: false ~some: Model.User.is_administrator'

let assert_can_administrate env f =
  let%lwt user = Environment.user env in
  Option.fold
    user
    ~none: (fun () ->
      Log.info (fun m -> m "Refusing admin access to %a." Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission to administrate this instance."
    )
    ~some: (fun user () ->
      if Model.User.is_administrator' @@ Database.User.entry_to_common user then
        f user
      else
        (
          Log.info (fun m -> m "Refusing admin access to %a." Environment.pp env);
          Madge_server.shortcut_forbidden "You do not have permission to administrate this instance."
        )
    )
    ()
