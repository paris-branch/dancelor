open NesUnix
open Common

module Log = (val Logger.create "database.storage": Logs.LOG)

let prefix = Config.database

module Git = struct
  let add path =
    Log.debug (fun m -> m "Running git add");
    Process.run_ignore ~cwd: !prefix ~on_wrong_status: Logs.Error ["git"; "add"; path]

  let commit ~msg =
    Log.debug (fun m -> m "Running git commit");
    Process.run_ignore
      ~cwd: !prefix
      ~on_wrong_status: Logs.Error
      [
        "git";
        "commit";
        "--author=Auto <noreply@dancelor.org>";
        ("--message=" ^ msg)
      ]

  let push () =
    Log.debug (fun m -> m "Running git push");
    Process.run_ignore ~cwd: !prefix ~on_wrong_status: Logs.Error ["git"; "push"]

  let pull_rebase () =
    Log.debug (fun m -> m "Running git pull rebase");
    Process.run_ignore ~cwd: !prefix ~on_wrong_status: Logs.Error ["git"; "pull"; "--rebase"]

  let status_clean () =
    Log.debug (fun m -> m "Checking for clean git status");
    let%lwt out =
      Process.run
        ~cwd: !prefix
        ~on_wrong_status: Logs.Error
        ~on_nonempty_stderr: Logs.Warning
        ["git"; "status"]
    in
    Lwt.return (out.Process.stdout = "")
end

module Json = struct
  include Json

  (* NOTE: We are building a [from_string] that understands all of
     [Yojson.Safe.t] but also if under a YAML format. *)

  let of_yaml_scalar (sc : Yaml.scalar) : t =
    match sc.style with
    | `Any -> invalid_arg "NesJson.of_yaml_scalar: unsupported scalar type: `Any"
    | `Literal -> invalid_arg "NesJson.of_yaml_scalar: unsupported scalar type: `Literal"
    | `Folded -> invalid_arg "NesJson.of_yaml_scalar: unsupported scalar type: `Folded"
    | `Single_quoted | `Double_quoted -> `String sc.value
    | `Plain ->
      match int_of_string_opt sc.value with
      | Some i -> `Int i
      | None ->
        match float_of_string_opt sc.value with
        | Some f -> `Float f
        | None ->
          if sc.value = "true" then `Bool true
          else if sc.value = "false" then `Bool false
          else `String sc.value

  let string_of_yaml : Yaml.yaml -> string = function
    | `Scalar sc -> sc.value
    | _ -> invalid_arg "NesJson.string_of_yaml: a non-scalar cannot be a string"

  let rec from_yaml : Yaml.yaml -> t = function
    | `Scalar sc -> of_yaml_scalar sc
    | `Alias _ -> invalid_arg "NesJson.of_yaml: unsupported aliases"
    | `A sequence -> `List (List.map from_yaml sequence.s_members)
    | `O mapping -> `Assoc (List.map (fun (k, v) -> (string_of_yaml k, from_yaml v)) mapping.m_members)

  let from_yaml_string str = str |> Yaml.yaml_of_string |> Result.get_ok |> from_yaml

  (* NOTE: `Yaml.value` is in fact the JSON subset of YAML as represented by the
     `yaml` library. This is more than enough for our use. *)
  let rec basic_to_yaml : Yojson.Basic.t -> Yaml.value = function
    | `Null -> `Null
    | `Bool b -> `Bool b
    | `Int i -> `Float (float_of_int i)
    | `Float f -> `Float f
    | `String s -> `String s
    | `List values -> `A (List.map basic_to_yaml values)
    | `Assoc bindings -> `O (List.map (fun (k, v) -> (k, basic_to_yaml v)) bindings)

  let to_yaml json = basic_to_yaml (Yojson.Safe.to_basic json)

  let to_yaml_string json = json |> to_yaml |> Yaml.to_string_exn
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
    Lwt_mutex.with_lock
      lock
      (fun () ->
         Log.debug (fun m -> m "Got lock[%x] on storage" id);
         f ()
      )
  in
  Log.debug (fun m -> m "Released lock[%x] on storage" id);
  Lwt.return a

let check_ro_lock () =
  if Lwt_mutex.is_locked ro_lock then
    Error.(lwt_fail StorageReadOnly)
  else
    Lwt.return_unit

let with_read_only f =
  (
    with_lock @@ fun () ->
    Log.debug (fun m -> m "Setting storage as read-only");
    Lwt_mutex.lock ro_lock
  );%lwt
  let%lwt y = f () in
  Log.debug (fun m -> m "Freeing the read-only lock");
  Lwt_mutex.unlock ro_lock;
  Lwt.return y

let with_locks (type a) (f : unit -> a Lwt.t) : a Lwt.t =
  with_lock @@ fun () ->
  check_ro_lock ();%lwt
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

let read_entry_yaml table entry file =
  (* no lock because using read_entry_file *)
  let%lwt content = read_entry_file table entry file in
  Json.from_yaml_string content
  |> Lwt.return

let write_entry_file table entry file content =
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Writing %s / %s / %s" table entry file);
  if !Config.write_storage then
    (
      let path = Filename.concat_l [!prefix; table; entry] in
      Filesystem.create_directory ~fail_if_exists: false path;
      let path = Filename.concat path file in
      Filesystem.write_file path content
    );
  Lwt.return ()

let write_entry_yaml table entry file content =
  (* no lock because using write_entry_file *)
  Json.to_yaml_string content
  |> write_entry_file table entry file

let delete_entry table entry =
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Deleting %s / %s" table entry);
  if !Config.write_storage then
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
  if !Config.write_storage then
    (
      (* no prefix for git! *)
      let path = Filename.concat_l [ (*!prefix;*) table; entry] in
      Git.add path;%lwt
      Git.commit ~msg;%lwt
      Lwt.return_unit
    )
  else
    Lwt.return_unit

let sync_changes () =
  with_locks @@ fun () ->
  Log.debug (fun m -> m "Syncing");
  if !Config.sync_storage then
    (
      Git.pull_rebase ();%lwt
      Git.push ();%lwt
      Lwt.return_unit
    )
  else
    Lwt.return_unit
