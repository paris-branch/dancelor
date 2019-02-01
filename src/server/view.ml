open Dancelor_common
module Log = (val Log.create "dancelor.server.view")

let in_prefix file = Filename.(concat (concat !Config.share "views") file)

let views = Hashtbl.create 8

let load_file file =
  if Filename.check_suffix file ".html" then
    (
      Log.debug (fun m -> m "Loading view file %s" file);
      let ichan = open_in (in_prefix file) in
      Hashtbl.add views
        (Filename.chop_suffix file ".html")
        (Lexing.from_channel ichan |> Mustache.parse_lx);
      close_in ichan
    )

let rec load_dir dir =
  Log.debug (fun m -> m "Loading view dir %s" dir);
  Sys.readdir (in_prefix dir)
  |> Array.iter
       (fun file ->
         let file = Filename.concat dir file in
         if Sys.is_directory (in_prefix file) then
           load_dir file
         else
           load_file file)

let () = load_dir "/"

let render view json =
  Log.debug (fun m -> m "Rendering %s" view);
  try
    Mustache.render
      (Mustache.concat [
           Hashtbl.find views "/header";
           Hashtbl.find views view;
           Hashtbl.find views "/footer" ])
      json
  with
    Not_found ->
    Log.err (fun m -> m "Could not render view %s" view);
    failwith "Dancelor_server.View.render"
