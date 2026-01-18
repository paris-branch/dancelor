open Nes
open Common

module Log = (val Logger.create "controller.permission": Logs.LOG)

let fold_user env =
  Option.fold' (Environment.user env)

let assert_ ~access_type ?(shortcut_forbidden = Madge_server.shortcut_forbidden) ~forbidden_message can env =
  if can env then
    (
      Log.debug (fun m -> m "Granting %s access to %a." access_type Environment.pp env);
      lwt_unit
    )
  else
    (
      Log.info (fun m -> m "Refusing %s access to %a." access_type Environment.pp env);
      shortcut_forbidden forbidden_message
    )

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

(** {2 Public elements} *)

(** Whether the given user can get public elements. As the name suggests, they
    are public, so anyone, including anonymous users. *)
let can_get_public _env (_ : 'value Entry.public) = true

(** Whether the given user can create public elements. This is any connected
    user. *)
let can_create_public = is_connected

(** Whether the given user can update public elements. Those are only database
    maintainers and administrators. We require a witness entry, which is not
    used, but is meant to force us to check that the entry exists, and that it
    is indeed public (at type-checking time). *)
(* FIXME: grace period during which anyone can edit *)
let can_update_public env (_ : 'value Entry.public) =
  fold_user
    env
    ~none: (const false)
    ~some: (fun user -> Model.User.is_maintainer' user || Model.User.is_administrator' user)

(** Whether the given user can delete public elements. Those are only database
    maintainers and administrators. This is the same as {!can_update_public}. *)
(* FIXME: grace period during which anyone can edit *)
let can_delete_public = can_update_public

(** {3 Assertions} *)

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

(** {2 Private elements} *)

(** Whether the given user can get private elements. This is everyone if the
    visibility is set like that, or the selected viewers if there are some, or
    the owners of the entry or an omniscient administrator. *)
let can_get_private env entry =
  let access = Entry.access entry in
  let meta_visibility = Entry.Access.Private.meta_visibility access in
  (meta_visibility = `Everyone)
  || fold_user
    env
    ~none: (const false)
    ~some: (fun user ->
      NEList.exists (Entry.Id.equal' (Entry.id user)) (Entry.Access.Private.owners access)
      || (match meta_visibility with `Select_viewers viewers -> NEList.exists (Entry.Id.equal' (Entry.id user)) viewers | _ -> false)
      || Model.User.is_omniscient_administrator' user
    )

(** Whether the given user can create “private” elements. This is anyone that is
    connected. *)
let can_create_private env =
  fold_user env ~none: (const false) ~some: (const true)

(** Whether the given user can update a specific private element. This is one of
    the owners of the entry, or an omniscient administrator. *)
let can_update_private env entry =
  fold_user
    env
    ~none: (const false)
    ~some: (fun user ->
      NEList.exists (Entry.Id.equal' (Entry.id user)) (Entry.(Access.Private.owners % access) entry)
      || Model.User.is_omniscient_administrator' user
    )

(** Whether the given user can delete a specific private element. This is one of
    the owners of the entry, or an administrator. This is exactly the same as
    {!can_update_private}. *)
let can_delete_private = can_update_private

(** {3 Assertions} *)

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

(** {2 Administrating} *)

let can_admin =
  fold_user ~none: (const false) ~some: Model.User.is_administrator'

let assert_can_admin env f =
  fold_user
    env
    ~none: (fun () ->
      Log.info (fun m -> m "Refusing admin access to %a." Environment.pp env);
      Madge_server.shortcut_forbidden "You do not have permission to administrate this instance."
    )
    ~some: (fun user ->
      if Model.User.is_administrator' user then
        f user
      else
        (
          Log.info (fun m -> m "Refusing admin access to %a." Environment.pp env);
          Madge_server.shortcut_forbidden "You do not have permission to administrate this instance."
        )
    )
