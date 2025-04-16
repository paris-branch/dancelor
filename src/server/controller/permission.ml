open Nes

module Log = (val Logger.create "controller.permission": Logs.LOG)

let fold_user env =
  Option.fold' Environment.(user @@ session env)

(** {2 Reading} *)

let can_get env _entry =
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_get env entry =
  if can_get env entry then Lwt.return_unit
  else
    (
      Log.info (fun m -> m "Refusing get access to entry `%a`." Slug.pp' (Common.Entry.slug entry));
      Madge_cohttp_lwt_server.shortcut' `Not_found
    )

(** {2 Creating} *)

let can_create env =
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_create env =
  if can_create env then Lwt.return_unit
  else
    (
      Log.info (fun m -> m "Refusing create access.");
      Madge_cohttp_lwt_server.shortcut' `Forbidden
    )

(** {2 Updating} *)

let can_update env _entry =
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_update env entry =
  if can_update env entry then Lwt.return_unit
  else
    (
      Log.info (fun m -> m "Refusing update access to entry `%a`." Slug.pp' (Common.Entry.slug entry));
      Madge_cohttp_lwt_server.shortcut' `Forbidden
    )

(** {2 Deleting} *)

let can_delete env _entry =
  fold_user ~none: (fun () -> false) ~some: (fun _user -> true) env

let assert_can_delete env entry =
  if can_delete env entry then Lwt.return_unit
  else
    (
      Log.info (fun m -> m "Refusing delete access to entry `%a`." Slug.pp' (Common.Entry.slug entry));
      Madge_cohttp_lwt_server.shortcut' `Forbidden
    )
