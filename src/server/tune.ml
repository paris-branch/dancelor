open Dancelor_common
open Dancelor_model
open Common
open Cohttp_lwt_unix

module Api =
  struct
    let get query =
      let slug = query_string query "slug" in
      try
        Tune.Database.get slug
        |> Tune.view
        |> Tune.view_to_jsonm
        |> (fun json -> success ["tune", json])
      with
        Not_found -> error "this tune does not exist"

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

    let png query =
      (* FIXME: dirty as fuck *)
      let slug = query_string query "slug" in
      let view = Tune.(Database.get slug |> view) in
      let lilypond = Mustache.render lilypond_png_template (`O ["tune", Tune.view_to_jsonm view]) in
      let dirname = Filename.concat (Unix.getenv "DANCELOR_CACHE") "tune" in
      let basename = view.Tune.slug in
      let ochan = open_out (Filename.concat dirname (basename ^ ".ly")) in
      output_string ochan lilypond;
      close_out ochan;
      let rc =
        Sys.command ("cd " ^ (escape_shell_argument dirname)
                     ^ " && lilypond -dresolution=110 -dbackend=eps --png " ^ (escape_shell_argument (basename ^ ".ly")))
      in
      assert (rc = 0);
      Server.respond_file ~fname:(Filename.concat dirname (basename ^ ".png")) ()
  end

module Html =
  struct
    let get query =
      let view = Filename.concat (Unix.getenv "DANCELOR_VIEWS") "tune.html" in
      let ichan = open_in view in
      let template = Lexing.from_channel ichan |> Mustache.parse_lx in
      close_in ichan;
      Api.get query
      |> Mustache.render template
      |> respond_html
  end
