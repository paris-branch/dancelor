open ExtPervasives
module Log = (val Log.create "dancelor.common.storage" : Logs.LOG)

let prefix = Config.database

let list_entries table =
  Log.debug (fun m -> m "Listing entries in %s" table);
  Filename.concat prefix table
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (ExtFilename.concat3 prefix table ||> Sys.is_directory)

let list_subentries table entry =
  Log.debug (fun m -> m "Listing subentries in %s / %s" table entry);
  ExtFilename.concat3 prefix table entry
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (ExtFilename.concat4 prefix table entry ||> Sys.is_directory)

let read_entry_file table entry file =
  Log.debug (fun m -> m "Reading %s / %s / %s" table entry file);
  ExtFilename.concat_l [prefix; table; entry; file]
  |> Filesystem.read_file

let read_entry_json table entry =
  read_entry_file table entry ||> Ezjsonm.from_string

let read_subentry_file table entry subentry file =
  Log.debug (fun m -> m "Reading %s / %s / %s / %s" table entry subentry file);
  ExtFilename.concat_l [prefix; table; entry; subentry; file]
  |> Filesystem.read_file

let read_subentry_json table entry subentry =
  read_subentry_file table entry subentry ||> Ezjsonm.from_string

let write_entry_file table entry file content =
  let path = ExtFilename.concat3 prefix table entry in
  Filesystem.create_directory ~fail_if_exists:false path;
  let path = Filename.concat path file in
  Filesystem.write_file path content

let write_entry_json table entry file =
  JsonHelpers.check_object
  ||> Ezjsonm.to_string
  ||> write_entry_file table entry file
