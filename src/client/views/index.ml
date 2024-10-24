module Page = Dancelor_client_page
open Dancelor_client_html

let create () =
  Page.make ~title: (S.const "") @@
    div
      [
        h2 [txt "Welcome to Dancelor!"];
        txt
          "Something will eventually get added here, but for now you just have \
         this little text. You might want to ";
        a ~a: [a_href "/explore"] [txt "go explore"];
        txt ".";
      ]
