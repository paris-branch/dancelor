open Html

let create () =
  Lwt.return @@
    Page.make
      ~title: (S.const "")
      [
        h2 [txt "Welcome to Dancelor!"];
        txt
          "Something will eventually get added here, but for now you just have \
         this little text. You might want to ";
        a ~a: [a_href "/explore"] [txt "go explore"];
        txt ".";
      ]
