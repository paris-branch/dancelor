open Tyxml.Html
open Nes

module Log = (val Logger.create "static": Logs.LOG)

let index =
  (* NOTE: We use the boot time of Dancelor to invalidate caching of
     `client.js`. This means that clients will have to re-download every time
     Dancelor starts, but that isn't so often, and most of the time it is
     because of a change where we would want to invalidate things anyway. *)
  let boot_time = Datetime.(to_string @@ now ()) in
  Format.asprintf "%a" (pp ()) @@
    html
      (
        head (title (txt "Dancelor")) [
          meta ~a: [a_charset "utf-8"] ();

          (* Style *)
          meta ~a: [a_name "viewport"; a_content "width=device-width, initial-scale=1, maximum-scale=1"] ();
          link ~rel: [`Stylesheet] ~href: ("/fonts.css?" ^ boot_time) ();
          link ~rel: [`Stylesheet] ~href: ("/style.css?" ^ boot_time) ();

          (* Favicon *)
          link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(32, 32)])] ~href: ("/favicon-32x32.png?" ^ boot_time) ();
          link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(16, 16)])] ~href: ("/favicon-16x16.png?" ^ boot_time) ();
          link ~rel: [`Manifest] ~href: ("/site.webmanifest?" ^ boot_time) ();

          (* Dancelor *)
          script ~a: [a_script_type `Javascript; a_src ("/client.js?" ^ boot_time)] (txt "");
        ]
      )
      (
        body
          [
            script
              ~a: [
                a_src "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js";
                a_integrity "sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz";
                a_crossorigin `Anonymous;
              ]
              (txt "");
          ]
      )

let serve path =
  let full_path = Filename.concat !Config.share path in
  if Sys.file_exists full_path && not (Sys.is_directory full_path) then
    (
      Log.debug (fun m -> m "Serving static file: <share>/%s" @@ String.ltrim ~chars: ['/'] path);
      (* Keep static files in cache for 30 days. *)
      let headers = Cohttp.Header.init_with "Cache-Control" "max-age=2592000" in
      Cohttp_lwt_unix.Server.respond_file ~headers ~fname: full_path ()
    )
  else
    (
      Log.debug (fun m -> m "Serving main file.");
      let headers = Cohttp.Header.of_list [("Content-Type", "text/html")] in
      Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: index ()
    )
