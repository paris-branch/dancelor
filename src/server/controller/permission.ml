open Nes
open Common

module Log = (val Logger.create "controller.permission": Logs.LOG)

let fold_user env =
  Option.fold' (Environment.user env)

(** {2 Connected} *)

let is_connected env =
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_is_connected env =
  if is_connected env then
    (
      Log.debug (fun m -> m "Granting access to %a." Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing access to %a." Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission."
    )

(** {2 Reading} *)

let can_get env entry =
  (* if the entry is public, anyone, otherwise, anyone that is connected *)
  fold_user
    env
    ~none: (fun () -> Entry.(privacy % meta) entry = Public)
    ~some: (fun _user -> true)

(** The rejection of {!asert_can_get}. May be used by external code to behave in
    the exact same way and avoid leaking information. *)
let reject_can_get () =
  Madge_server.shortcut_not_found "This entry does not exist, or you do not have access to it."

let assert_can_get env entry =
  if can_get env entry then
    (
      Log.debug (fun m -> m "Granting get access for entry `%a` to %a." Entry.Id.pp' (Common.Entry.id entry) Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing get access for entry `%a` to %a." Entry.Id.pp' (Common.Entry.id entry) Environment.pp env);
      reject_can_get ()
    )

(** {2 Creating} *)

let can_create env =
  (* anyone that is connected *)
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_create env =
  if can_create env then
    (
      Log.debug (fun m -> m "Granting create access to %a." Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing create access to %a." Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission to create this entry."
    )

(** {2 Updating} *)

let can_update env _entry =
  (* anyone that is connected *)
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_update env entry =
  if can_update env entry then
    (
      Log.debug (fun m -> m "Granting update access for entry `%a` to %a." Entry.Id.pp' (Common.Entry.id entry) Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing update access for entry `%a` to %a." Entry.Id.pp' (Common.Entry.id entry) Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission to update this entry."
    )

(** {2 Deleting} *)

let can_delete env _entry =
  (* anyone that is connected *)
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_delete env entry =
  if can_delete env entry then
    (
      Log.debug (fun m -> m "Granting delete access for entry `%a` to %a." Entry.Id.pp' (Common.Entry.id entry) Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing delete access for entry `%a` to %a." Entry.Id.pp' (Common.Entry.id entry) Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission to delete this entry."
    )

(** {2 Administrating} *)

let can_admin =
  fold_user ~none: (const false) ~some: Model.User.admin

let assert_can_admin env =
  if can_admin env then
    (
      Log.debug (fun m -> m "Granting admin access to %a." Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing admin access to %a." Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission to administrate this instance."
    )
