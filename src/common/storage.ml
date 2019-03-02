open ExtPervasives
module Log = (val Log.create "dancelor.common.storage" : Logs.LOG)

let prefix = Config.database

module Shell = struct
  let command =
    List.map escape_shell_argument
    ||> String.concat " "

  let chdir path cmd =
    (command ["cd"; path]) ^ " && " ^ cmd

  let ignore command =
    command ^ " >/dev/null 2>&1"

  let c cmd =
    let cmd =
      command cmd |> chdir !prefix |> ignore (*FIXME: no ignore*)
    in
    Log.debug (fun m -> m "shell: %s" cmd);
    match Sys.command cmd with
    | 0 -> ()
    | rc -> Log.warn (fun m -> m "`%s` failed with return code: %d" cmd rc)
end

module Git = struct
  let add path =
    Shell.c ["git"; "add"; path]

  let commit ~msg =
    Shell.c ["git"; "commit"; "-m"; msg]

  let push () =
    Shell.c ["git"; "push"]

  let pull_rebase () =
    Shell.c ["git"; "pull"; "--rebase"]
end

let list_entries table =
  Log.debug (fun m -> m "Listing entries in %s" table);
  Filename.concat !prefix table
  |> Filesystem.read_directory
  |> List.filter (ExtFilename.concat3 !prefix table ||> Sys.is_directory)

let list_entry_files table entry =
  Log.debug (fun m -> m "Listing entries in %s / %s" table entry);
  ExtFilename.concat_l [!prefix; table; entry]
  |> Filesystem.read_directory

let read_entry_file table entry file =
  Log.debug (fun m -> m "Reading %s / %s / %s" table entry file);
  ExtFilename.concat_l [!prefix; table; entry; file]
  |> Filesystem.read_file

let read_entry_json table entry =
  read_entry_file table entry ||> Json.from_string

let write_entry_file table entry file content =
  let path = ExtFilename.concat3 !prefix table entry in
  Filesystem.create_directory ~fail_if_exists:false path;
  let path = Filename.concat path file in
  Filesystem.write_file path content

let write_entry_json table entry file =
  Json.to_string
  ||> write_entry_file table entry file

let delete_entry table entry =
  let path = ExtFilename.concat_l [!prefix; table; entry] in
  Filesystem.read_directory path
  |> List.iter (fun s -> Filesystem.remove_file (path ^ "/" ^ s));
  Filesystem.remove_directory path

let save_changes_on_entry ~msg table entry =
  (* no prefix for git! *)
  let path = ExtFilename.concat_l [(*!prefix;*) table; entry] in
  Git.add path;
  Git.commit ~msg

let sync_changes () =
  Git.pull_rebase ();
  Git.push ()
