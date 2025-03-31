open Tyxml.Html

module Log = (val Logger.create "static": Logs.LOG)

let index =
  Format.asprintf "%a" (pp ()) @@
  html
    (
      head
        (title (txt "Dancelor"))
        [
          meta ~a: [a_charset "utf-8"] ();

          (* Style *)
          meta ~a: [a_name "viewport"; a_content "width=device-width, initial-scale=1, maximum-scale=1"] ();
          link ~rel: [`Stylesheet] ~href: "/fonts.css" ();
          link ~rel: [`Stylesheet] ~href: "/style.css" ();

          (* Favicon *)
          link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(32, 32)])] ~href: "/favicon-32x32.png" ();
          link ~rel: [`Icon] ~a: [a_mime_type "image/png"; a_sizes (Some [(16, 16)])] ~href: "/favicon-16x16.png" ();
          link ~rel: [`Manifest] ~href: "/site.webmanifest" ();

          (* Dancelor *)
          script ~a: [a_script_type `Javascript; a_src "/client.js"] (txt "");
        ]
    )
    (body [])

let serve_index () =
  Log.debug (fun m -> m "Serving main file.");
  let headers = Cohttp.Header.of_list [("Content-Type", "text/html")] in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status: `OK ~body: index ()
