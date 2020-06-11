open NesUnix
module Log = (val Dancelor_server_logs.create "server.database.unsafe.storage" : Logs.LOG)

let prefix = Dancelor_server_config.database

module Git = struct
  let add path =
    Process.run_silent ["git"; "add"; path]

  let commit ~msg =
    Process.run_silent ["git"; "commit"; "-m"; msg]

  let push () =
    Process.run_silent ["git"; "push"]

  let pull_rebase () =
    Process.run_silent ["git"; "pull"; "--rebase"]
end

let list_entries table =
  Log.debug (fun m -> m "Listing entries in %s" table);
  Filename.concat !prefix table
  |> Filesystem.read_directory
  |> List.filter (fun dir -> Filename.concat_l [!prefix; table; dir] |> Sys.is_directory)

let list_entry_files table entry =
  Log.debug (fun m -> m "Listing entries in %s / %s" table entry);
  Filename.concat_l [!prefix; table; entry]
  |> Filesystem.read_directory

let read_entry_file table entry file =
  Log.debug (fun m -> m "Reading %s / %s / %s" table entry file);
  Filename.concat_l [!prefix; table; entry; file]
  |> Filesystem.read_file

let read_entry_json table entry =
  read_entry_file table entry ||> Json.from_string

let write_entry_file table entry file content =
  if !Dancelor_server_config.write_storage then
    (
      let path = Filename.concat_l [!prefix; table; entry] in
      Filesystem.create_directory ~fail_if_exists:false path;
      let path = Filename.concat path file in
      Filesystem.write_file path content
    )

let write_entry_json table entry file =
  Json.to_string
  ||> write_entry_file table entry file

let delete_entry table entry =
  if !Dancelor_server_config.write_storage then
    (
      let path = Filename.concat_l [!prefix; table; entry] in
      Filesystem.read_directory path
      |> List.iter (fun s -> Filesystem.remove_file (path ^ "/" ^ s));
      Filesystem.remove_directory path
    )

let save_changes_on_entry ~msg table entry =
  if !Dancelor_server_config.write_storage then
    (
      (* no prefix for git! *)
      let path = Filename.concat_l [(*!prefix;*) table; entry] in
      let%lwt () = Git.add path in
      let%lwt () = Git.commit ~msg in
      Lwt.return_unit
    )
  else
    Lwt.return_unit

let sync_changes () =
  if !Dancelor_server_config.sync_storage then
    (
      let%lwt () = Git.pull_rebase () in
      let%lwt () = Git.push () in
      Lwt.return_unit
    )
  else
    Lwt.return_unit
