open Tyxml.Html
open Nes
open Common

module Log = (val Logger.create "static": Logs.LOG)

let base_url = Uri.of_string "https://dancelor.org"

(* NOTE: We use the boot time of Dancelor to invalidate caching of `client.js`.
   This means that clients will have to re-download every time Dancelor starts,
   but that isn't so often, and most of the time it is because of a change where
   we would want to invalidate things anyway. *)
let index =
  let boot_time = Datetime.to_string Environment.boot_time in
  Format.asprintf "%a" (pp ()) @@
    html
      (
        head (title (txt "Dancelor")) [
          meta ~a: [a_charset "utf-8"] ();

          (* Style *)
          meta ~a: [a_name "viewport"; a_content "width=device-width, initial-scale=1, maximum-scale=1"] ();
          meta ~a: [a_name "description"; a_content "Dancelor — Scottish country dance music database for managing tunes, sets, books, and more."] ();
          link ~rel: [`Stylesheet] ~href: ("/fonts.css?" ^ boot_time) ();
          link ~rel: [`Stylesheet] ~href: ("/style.css?" ^ boot_time) ();

          (* Favicon *)
          link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(32, 32)])] ~href: ("/favicon-32x32.png?" ^ boot_time) ();
          link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(16, 16)])] ~href: ("/favicon-16x16.png?" ^ boot_time) ();
          link ~rel: [`Manifest] ~href: ("/site.webmanifest?" ^ boot_time) ();

          (* Ahrefs ownership proof and analytics *)
          meta ~a: [a_name "ahrefs-site-verification"; a_content "4c418f04303adf3925d1c6bdef51b71cab26cd179ab478ecd1996cf71ce52ba4"] ();
          script ~a: [a_src "https://analytics.ahrefs.com/analytics.js"; a_user_data "key" "HmcihempNgdCWYAHGvIYsg"; a_async ()] (txt "");

          (* Dancelor *)
          script ~a: [a_script_type `Javascript; a_src ("/client.js?" ^ boot_time)] (txt "");
        ]
      )
      (
        body
          ~a: [a_class ["placeholder-glow"]]
          [
            script ~a: [a_src "/bootstrap.bundle.min.js"] (txt "");
          ]
      )

let static_pages = ["/"; "/explore"]

let serve_sitemap env =
  Log.info (fun m -> m "Generating and serving sitemap.xml");
  let%lwt (_, anys) = Controller.Any.search env Slice.everything Formula.true_ in
  let urls = static_pages @ List.map Endpoints.Page.href_any_full anys in
  let urls = List.map (fun path -> Uri.with_uri ~path: (Some path) base_url) urls in
  let%lwt sitemap =
    let buf = Buffer.create 10240 in
    let fmt = Format.formatter_of_buffer buf in
    fpf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
    fpf fmt {|<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">|};
    (* FIXME: Probably that the update frequency should depend on what we're talking
       about. The explore page might change very often but the items themselves
       should change pretty rarely. For now, we put weekly everywhere. *)
    List.iter (fpf fmt {|<url><loc>%s</loc><changefreq>weekly</changefreq></url>|} % Uri.to_string) urls;
    fpf fmt {|</urlset>|};
    Format.pp_print_flush fmt ();
    lwt @@ Buffer.contents buf
  in
  let headers =
    Cohttp.Header.of_list [
      ("Content-Type", "application/xml");
      ("Cache-Control", "max-age=3600");
    ]
  in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: sitemap ()

let serve_robots_txt () =
  Log.info (fun m -> m "Serving robots.txt");
  let robots_txt = spf "Sitemap: %s/sitemap.xml\nUser-agent: *\nAllow: /\n" (Uri.to_string base_url) in
  let headers =
    Cohttp.Header.of_list [
      ("Content-Type", "text/plain");
      ("Cache-Control", "max-age=3600");
    ]
  in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: robots_txt ()

let serve env path =
  let full_path = Filename.concat !Config.share path in
  if Sys.file_exists full_path && not (Sys.is_directory full_path) then
    (
      Log.debug (fun m -> m "Serving static file: <share>/%s" @@ String.ltrim ~chars: ['/'] path);
      (* Keep static files in cache for 30 days. *)
      let headers = Cohttp.Header.init_with "Cache-Control" "max-age=2592000" in
      Cohttp_lwt_unix.Server.respond_file ~headers ~fname: full_path ()
    )
  else if path = "/sitemap.xml" then
    serve_sitemap env
  else if path = "/robots.txt" then
    serve_robots_txt ()
  else
    (
      Log.debug (fun m -> m "Serving main file.");
      let headers = Cohttp.Header.of_list [("Content-Type", "text/html")] in
      Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: index ()
    )
