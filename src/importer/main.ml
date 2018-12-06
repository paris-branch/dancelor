open Dancelor_common
open Dancelor_model

let db = "/home/niols/git/perso/rscds/db"
let tunes k = Filename.(concat db (concat "tune" k))

(* FIXME: reel, jig, strathspey, waltz *)

let read_file path =
  let ic = open_in path in
  let bufsize = 1024 in
  let buf = Bytes.create bufsize in
  let out = Buffer.create bufsize in
  let rec aux () =
    match input ic buf 0 bufsize with
    | 0 -> ()
    | n ->
       Buffer.add_subbytes out buf 0 n;
       aux ()
  in
  aux ();
  close_in ic;
  Buffer.contents out

let find_opus content =
  let opus_regexp = Str.regexp "opus = \"\\(.*\\)\"" in
  try
    ignore (Str.search_forward opus_regexp content 0);
    Str.matched_group 1 content
  with
    Not_found -> ""

let find_key content =
  let regexp = Str.regexp "\\\\key \\(.*\\) \\\\\\([a-z]*\\)[ \n\t\r]" in
  try
    ignore (Str.search_forward regexp content 0);
    (Music.pitch_of_string (Str.matched_group 1 content),
     match Str.matched_group 2 content with
     | "major" -> Music.Major
     | "minor" -> Music.Minor
     | _ -> failwith "find_key")
  with
    Not_found -> Music.((C, Natural), Major)

let remove_score_and_headers content =
  Format.eprintf "remove_score_and_headers working on:@.%s@." content;
  let regexp = Str.regexp "\\\\score[ \t\r\n]*{[ \t\r\n]*\\\\header[ \t\r\n]*{[^}]*}" in
  let content' = Str.replace_first regexp "" content in
  if content' <> content then
    (
      let content' = String.trim content' in
      if content'.[String.length content' - 1] = '}' then
        (
          let content' = String.sub content' 0 (String.length content' - 1) in
          Format.eprintf "got:@.%s@." content';
          content'
        )
      else
        failwith "remove_score_and_headers"
    )
  else
    failwith "remove_score_and_headers"

let do_one kind_str name index path disambiguation =
  let slug = (Slug.from_string name) ^ "-" ^ (string_of_int index) in
  let disambiguation =
    if disambiguation = "default" then
      None
    else
      Some disambiguation
  in
  let kind = (32, Kind.base_of_string kind_str) in
  let content = read_file path in
  if content <> "" then
    (
      let author = Credit.make ~line:(find_opus content) () in
      let content = remove_score_and_headers content in
      let key = find_key content in
      let tune =
        Tune.make
          ~slug ~name ?disambiguation
          ~kind ~key
          ~author ~content ()
      in
      Format.printf "%s@." (Yaml.to_string_exn (`O ["tune", Tune.to_yaml tune]))
    )

let do_ kind_str =
  Sys.readdir (tunes kind_str)
  |> Array.iter
       (fun name ->
         let path = Filename.concat (tunes kind_str) name in
         Sys.readdir path
         |> Array.iteri
              (fun i file ->
                if Filename.check_suffix file ".ly" then
                  do_one kind_str name i (Filename.concat path file) (Filename.chop_extension file)))

let main () =
  do_ "reel";
  do_ "jig";
  do_ "strathspey";
  do_ "waltz"

let () = main ()
