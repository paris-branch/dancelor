open Dancelor_common
open Dancelor_model
open QueryHelpers

let get query _body =
  let slug = query_string query "slug" in
  try
    Tune.Database.get slug
    |> Tune.view
    |> Tune.view_to_jsonm
    |> (fun json -> Lwt.return (`O ["tune", json]))
  with
    Not_found ->
    raise (Error.Error (`OK, "this tune does not exist"))

let get_all query _body =
  let tune_jsons =
    Tune.Database.get_all
      ~name:(query_string_or query "name" "")
      ~author:(query_string_or query "author" "")
      ?kind:(
        let kind = query_string_or query "kind" "" in
        if kind = "" then
          None
        else
          try
            Some (Kind.base_of_string kind)
          with
            Failure _ -> raise (Error.Error (`OK, "kind must be 'j', 'p', 'r', 's' or 'w'"))
      )
      ?keys:(
        let open OptionMonad in
        query_strings_opt query "keys" >>= fun keys ->
        Some (List.map Music.key_of_string keys)
      )
      ?mode:(
        let open OptionMonad in
        query_string_opt query "mode" >>= function
        | "major" -> Some Music.Major
        | "minor" -> Some Music.Minor
        | _ -> error ("mode must be major or minor")
      )
      ()
    |> List.map (fun (score, tune) ->
           Tune.(tune |> view |> view_to_jsonm)
           |> JsonHelpers.add_field "score" (`Float (100. *. score)))
    |> (fun jsons -> `A jsons)
  in
  Lwt.return (
      `O [
          "tunes", tune_jsons;
          "query",
          `O [
              "name", `String (query_string_or query "name" "");
              "author", `String (query_string_or query "author" "");
            ]
        ]
    )

let lilypond_png_template =
  Mustache.of_string
    "\\version \"2.19.82\"

     \\header {
     tagline = \"\"
     }

     \\paper {
     indent = 0
     }

     \\score {
     {{{tune.content}}}
     }"

let png query _body =
  (* FIXME: dirty as fuck *)
  let slug = query_string query "slug" in
  let view = Tune.(Database.get slug |> view) in
  let lilypond = Mustache.render lilypond_png_template (`O ["tune", Tune.view_to_jsonm view]) in
  let dirname = Filename.concat (Config.cache_prefix ()) "tune" in
  let basename = view.Tune.slug in
  let ochan = open_out (Filename.concat dirname (basename ^ ".ly")) in
  output_string ochan lilypond;
  close_out ochan;
  let rc =
    Sys.command ("cd " ^ (escape_shell_argument dirname)
                 ^ " && lilypond -dresolution=110 -dbackend=eps --png "
                 ^ (escape_shell_argument (basename ^ ".ly")))
  in
  assert (rc = 0);
  Cohttp_lwt_unix.Server.respond_file ~fname:(Filename.concat dirname (basename ^ ".png")) ()
