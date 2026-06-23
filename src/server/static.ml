open Tyxml.Html
open Nes
open Dancelor_common
open Search_new

module Log = (val Logs.src_log @@ Logs.Src.create "server.static": Logs.LOG)

let base_url = Uri.of_string "https://dancelor.org"

let static_file_path path = Filename.concat (Config.get ()).share path

let serve_static_file path =
  let full_path = static_file_path path in
  if Sys.file_exists full_path && not (Sys.is_directory full_path) then
    Some (fun () ->
      (* Keep static files in cache for 30 days. *)
      let headers = Cohttp.Header.init_with "Cache-Control" "max-age=2592000" in
      Cohttp_lwt_unix.Server.respond_file ~headers ~fname: full_path ()
    )
  else
    None

let href_with_hash path =
  spf "/%s?%s" path @@ Digest.BLAKE256.(to_hex % file) @@ static_file_path path

let serve_index =
  (* NOTE: We add the hash of static files to their URL to invalidate
     cache (in particular `client.js`). Otherwise, we cache them for
     as long as possible. *)
  let fonts_css_href = Lazy.from_fun @@ fun () -> href_with_hash "fonts.css" in
  let style_css_href = Lazy.from_fun @@ fun () -> href_with_hash "style.css" in
  let favicon_16x16_png_href = Lazy.from_fun @@ fun () -> href_with_hash "favicon-16x16.png" in
  let favicon_32x32_png_href = Lazy.from_fun @@ fun () -> href_with_hash "favicon-32x32.png" in
  let site_webmanifest_href = Lazy.from_fun @@ fun () -> href_with_hash "site.webmanifest" in
  let client_js_href = Lazy.from_fun @@ fun () -> href_with_hash "client.js" in
  let bootstrap_bundle_min_js_href = Lazy.from_fun @@ fun () -> href_with_hash "bootstrap.bundle.min.js" in
  fun path ->
    let index =
      Format.asprintf "%a" (pp ()) @@
        html
          ~a: [a_lang "en"]
          (
            head (title (txt "Dancelor")) [
              meta ~a: [a_charset "utf-8"] ();
              link ~rel: [`Canonical] ~href: (Uri.to_string @@ Uri.with_path base_url path) ();
              (* Style *)
              meta ~a: [a_name "viewport"; a_content "width=device-width, initial-scale=1, maximum-scale=1"] ();
              meta ~a: [a_name "description"; a_content "Dancelor — A community-edited database of Scottish country dance music. Search for tunes, assemble sets and books, and export to PDF, ready to print and bring to the dance."] ();
              link ~rel: [`Stylesheet] ~href: (Lazy.force fonts_css_href) ();
              link ~rel: [`Stylesheet] ~href: (Lazy.force style_css_href) ();
              (* Favicon *)
              link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(32, 32)])] ~href: (Lazy.force favicon_32x32_png_href) ();
              link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(16, 16)])] ~href: (Lazy.force favicon_16x16_png_href) ();
              link ~rel: [`Manifest] ~href: (Lazy.force site_webmanifest_href) ();
              (* Ahrefs ownership proof and analytics *)
              meta ~a: [a_name "ahrefs-site-verification"; a_content "4c418f04303adf3925d1c6bdef51b71cab26cd179ab478ecd1996cf71ce52ba4"] ();
              script ~a: [a_src "https://analytics.ahrefs.com/analytics.js"; a_user_data "key" "HmcihempNgdCWYAHGvIYsg"; a_async ()] (txt "");
              (* Dancelor *)
              script ~a: [a_script_type `Javascript; a_src (Lazy.force client_js_href)] (txt "");
            ]
          )
          (
            body
              ~a: [a_class ["placeholder-glow"]]
              [
                script ~a: [a_src (Lazy.force bootstrap_bundle_min_js_href)] (txt "");
              ]
          )
    in
    let headers = Cohttp.Header.of_list [("Content-Type", "text/html")] in
    Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: index ()

let static_pages = List.map Uri.of_string ["/"; "/explore"]

let serve_sitemap env =
  let%lwt anys = Search_result.items <$> Controller.Any.search env Slice.everything (Any_query.empty) in
  let urls = static_pages @ List.map (Endpoints.Page.href_any_full_new % Model_new.Any_row.to_id) anys in
  let urls = List.map (fun url -> Uri.with_path base_url (Uri.path url)) urls in
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
  let robots_txt = spf "Sitemap: %s/sitemap.xml\nUser-agent: *\nAllow: /\n" (Uri.to_string base_url) in
  let headers =
    Cohttp.Header.of_list [
      ("Content-Type", "text/plain");
      ("Cache-Control", "max-age=3600");
    ]
  in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: robots_txt ()

let serve env path =
  Log.debug (fun m -> m "Looking to serve %S" path);
  match serve_static_file path with
  | Some serve_static_file ->
    (
      Log.debug (fun m -> m "Serving static file: <share>/%s" @@ String.ltrim ~chars: ['/'] path);
      serve_static_file ()
    )
  | None ->
    if path = "/sitemap.xml" then
      (
        Log.debug (fun m -> m "Generating and serving sitemap.xml");
        serve_sitemap env
      )
    else if path = "/robots.txt" then
      (
        Log.debug (fun m -> m "Serving robots.txt");
        serve_robots_txt ()
      )
    else
      (
        Log.debug (fun m -> m "Serving main file.");
        serve_index path
      )
