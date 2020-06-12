open NesUnix
module Log = (val Dancelor_server_logs.create "database.storage" : Logs.LOG)

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

let lock = Lwt_mutex.create ()
let with_lock (type a) (f : unit -> a Lwt.t) : a Lwt.t =
  let id = Random.int (1 lsl 28) in
  Log.debug (fun m -> m "Waiting for lock[%x] on storage" id);
  let%lwt a =
    Lwt_mutex.with_lock lock
      (fun () ->
         Log.debug (fun m -> m "Got lock[%x] on storage" id);
         f ())
  in
  Log.debug (fun m -> m "Released lock[%x] on storage" id);
  Lwt.return a

(* Lock is required for all functions that manipulate the filesystem directly. *)

let list_entries table =
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Listing entries in %s" table);
  Filename.concat !prefix table
  |> Filesystem.read_directory
  |> List.filter (fun dir -> Filename.concat_l [!prefix; table; dir] |> Sys.is_directory)
  |> Lwt.return

let list_entry_files table entry =
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Listing entries in %s / %s" table entry);
  Filename.concat_l [!prefix; table; entry]
  |> Filesystem.read_directory
  |> Lwt.return

let read_entry_file table entry file =
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Reading %s / %s / %s" table entry file);
  Filename.concat_l [!prefix; table; entry; file]
  |> Filesystem.read_file
  |> Lwt.return

let read_entry_json table entry file =
  (* no lock because using read_entry_file *)
  let%lwt content = read_entry_file table entry file in
  Json.from_string content
  |> Lwt.return

let write_entry_file table entry file content =
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Writing %s / %s / %s" table entry file);
  if !Dancelor_server_config.write_storage then
    (
      let path = Filename.concat_l [!prefix; table; entry] in
      Filesystem.create_directory ~fail_if_exists:false path;
      let path = Filename.concat path file in
      Filesystem.write_file path content
    );
  Lwt.return ()

let write_entry_json table entry file content =
  (* no lock because using write_entry_file *)
  Json.to_string content
  |> write_entry_file table entry file

let delete_entry table entry =
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Deleting %s / %s" table entry);
  if !Dancelor_server_config.write_storage then
    (
      let path = Filename.concat_l [!prefix; table; entry] in
      Filesystem.read_directory path
      |> List.iter (fun s -> Filesystem.remove_file (path ^ "/" ^ s));
      Filesystem.remove_directory path
    );
  Lwt.return_unit

let save_changes_on_entry ~msg table entry =
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Saving %s / %s" table entry);
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
  with_lock @@ fun () ->
  Log.debug (fun m -> m "Syncing");
  if !Dancelor_server_config.sync_storage then
    (
      let%lwt () = Git.pull_rebase () in
      let%lwt () = Git.push () in
      Lwt.return_unit
    )
  else
    Lwt.return_unit
