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
      let view = Tune.(Database.get slug |> view |> view_to_jsonm) in
      let lilypond = Mustache.render lilypond_png_template (`O ["tune", view]) in
      Format.eprintf "command starts@.";
      Sys.command
        (Format.sprintf
           "printf -- '%%s' %s | lilypond -dresolution=110 -dbackend=eps --png -"
           (escape_shell_argument lilypond))
      |> ignore;
      Format.eprintf "command ends@.";
      Server.respond_file ~fname:"-.png" ()
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
