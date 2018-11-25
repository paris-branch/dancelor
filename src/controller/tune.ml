open Dancelor_common
open Dancelor_model
open LwtResultMonad
open QueryHelpers

let get query =
  Lwt.return (query_string query "slug") >>= fun slug ->
  try
    Tune.Database.get slug
    |> Tune.view
    |> Tune.view_to_jsonm
    |> (fun json -> Lwt.return (Ok (`O ["tune", json])))
  with
    Not_found ->
    Lwt.return (Error "this tune does not exist")

(* let lilypond_png_template =
 *   Mustache.of_string
 *     "\\version \"2.19.82\"
 *
 *      \\header {
 *      tagline = \"\"
 *      }
 *
 *      \\paper {
 *      indent = 0
 *      }
 *
 *      \\score {
 *      {{{tune.content}}}
 *      }"
 *
 * let png query =
 *   (\* FIXME: dirty as fuck *\)
 *   let slug = query_string query "slug" in
 *   let view = Tune.(Database.get slug |> view) in
 *   let lilypond = Mustache.render lilypond_png_template (`O ["tune", Tune.view_to_jsonm view]) in
 *   let dirname = Filename.concat (Unix.getenv "DANCELOR_CACHE") "tune" in
 *   let basename = view.Tune.slug in
 *   let ochan = open_out (Filename.concat dirname (basename ^ ".ly")) in
 *   output_string ochan lilypond;
 *   close_out ochan;
 *   let rc =
 *     Sys.command ("cd " ^ (escape_shell_argument dirname)
 *                  ^ " && lilypond -dresolution=110 -dbackend=eps --png "
 *                  ^ (escape_shell_argument (basename ^ ".ly")))
 *   in
 *   assert (rc = 0);
 *   Server.respond_file ~fname:(Filename.concat dirname (basename ^ ".png")) () *)
