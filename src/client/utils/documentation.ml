open Nes
open Html

let link fname =
  a ~a: [a_href @@ Uri.of_string @@ spf "https://github.com/paris-branch/dancelor/blob/main/doc/%s.md" fname] [
    (* FIXME: hovering over the button should show a short help,
       which we can compute from the help page constructor *)
    Icon.html (Other Help);
  ];
