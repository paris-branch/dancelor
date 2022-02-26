open NesUnix
module Log = (val Dancelor_server_logs.create "database.storage" : Logs.LOG)

let prefix = Dancelor_server_config.database

module Git = struct
  let add path =
    Log.debug (fun m -> m "Running git add");
    Process.run_ignore ~cwd:!prefix ~on_wrong_status:Logs.Error ["git"; "add"; path]

  let commit ~msg =
    Log.debug (fun m -> m "Running git commit");
    Process.run_ignore ~cwd:!prefix ~on_wrong_status:Logs.Error [
      "git"; "commit";
      "--author=Auto <noreply@dancelor.org>";
      ("--message="^msg)
    ]

  let push () =
    Log.debug (fun m -> m "Running git push");
    Process.run_ignore ~cwd:!prefix ~on_wrong_status:Logs.Error ["git"; "push"]

  let pull_rebase () =
    Log.debug (fun m -> m "Running git pull rebase");
    Process.run_ignore ~cwd:!prefix ~on_wrong_status:Logs.Error ["git"; "pull"; "--rebase"]

  let status_clean () =
    Log.debug (fun m -> m "Checking for clean git status");
    let%lwt out =
      Process.run
        ~cwd:!prefix
        ~on_wrong_status:Logs.Error ~on_nonempty_stderr:Logs.Warning
        ["git"; "status"]
    in
    Lwt.return (out.Process.stdout = "")
end

(* Two locks. One is local to storage functions and is simply here to ensure
   that the storage is being written or read by only one function at a time. For
   that lock, functions *)

let lock = Lwt_mutex.create ()
let ro_lock = Lwt_mutex.create ()

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

let check_ro_lock () =
  if Lwt_mutex.is_locked ro_lock then
    Dancelor_common.Error.(lwt_fail `StorageReadOnly)
  else
    Lwt.return_unit

let with_read_only f =
  (
    with_lock @@ fun () ->
    Log.debug (fun m -> m "Setting storage as read-only");
    Lwt_mutex.lock ro_lock
  ); %lwt
  let%lwt y = f () in
  Log.debug (fun m -> m "Freeing the read-only lock");
  Lwt_mutex.unlock ro_lock;
  Lwt.return y

let with_locks (type a) (f : unit -> a Lwt.t) : a Lwt.t =
  with_lock @@ fun () ->
  check_ro_lock (); %lwt
  f ()

(* Lock is required for all functions that manipulate the filesystem directly. *)

let list_entries table =
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Listing entries in %s" table);
  Filename.concat !prefix table
  |> Filesystem.read_directory
  |> List.filter (fun dir -> Filename.concat_l [!prefix; table; dir] |> Sys.is_directory)
  |> Lwt.return

let list_entry_files table entry =
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Listing entries in %s / %s" table entry);
  Filename.concat_l [!prefix; table; entry]
  |> Filesystem.read_directory
  |> Lwt.return

let read_entry_file table entry file =
  with_locks @@ fun () ->
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
  with_locks @@ fun () ->
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
  with_locks @@ fun () ->
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
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Saving %s / %s" table entry);
  if !Dancelor_server_config.write_storage then
    (
      (* no prefix for git! *)
      let path = Filename.concat_l [(*!prefix;*) table; entry] in
      Git.add path; %lwt
      Git.commit ~msg; %lwt
      Lwt.return_unit
    )
  else
    Lwt.return_unit

let sync_changes () =
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Syncing");
  if !Dancelor_server_config.sync_storage then
    (
      Git.pull_rebase (); %lwt
      Git.push (); %lwt
      Lwt.return_unit
    )
  else
    Lwt.return_unit
