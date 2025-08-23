open NesUnix
open Common

module Log = (val Logger.create "controller.book.pdf": Logs.LOG)

let cache : ([`Pdf] * Model.Book.t * Model.BookParameters.t * string * RenderingParameters.t, string Lwt.t) StorageCache.t =
  StorageCache.create ()

let populate_cache () =
  ControllerCache.populate ~cache ~type_: "book" ~ext: ".pdf" ~pp_ext: "pdf"

let render book book_parameters rendering_parameters =
  let%lwt body = Model.Book.lilypond_contents_cache_key book in
  StorageCache.use ~cache ~key: (`Pdf, book, book_parameters, body, rendering_parameters) @@ fun hash ->
  let%lwt rendering_parameters =
    let%lwt pdf_metadata =
      let name = Model.Book.title book in
      let%lwt composers = List.map (NEString.to_string % Model.Person.name') <$> Model.Book.authors book in
      lwt @@
        RenderingParameters.update_pdf_metadata
          ~title: (String.replace_empty ~by: name)
          ~composers: (List.replace_nil ~by: composers)
    in
    lwt @@
      RenderingParameters.update ~pdf_metadata rendering_parameters
  in
  let%lwt lilypond = Ly.render book book_parameters rendering_parameters in
  let path = Filename.concat !Config.cache "book" in
  let (fname_ly, fname_pdf) =
    let fname = StorageCache.hash_to_string hash in
      (fname ^ ".ly", fname ^ ".pdf")
  in
  Lwt_io.with_file
    ~mode: Output
    (Filename.concat path fname_ly)
    (fun ochan -> Lwt_io.write ochan lilypond);%lwt
  Log.debug (fun m -> m "Processing with LilyPond");
  LilyPond.run
    ~exec_path: path
    ~fontconfig_file: (Filename.concat !Config.share "fonts.conf")
    fname_ly;%lwt
  let path_pdf = Filename.concat path fname_pdf in
  lwt path_pdf

let get env id _slug book_parameters rendering_parameters =
  match%lwt Model.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get env book;%lwt
    let%lwt path_pdf = render (Entry.value book) book_parameters rendering_parameters in
    Madge_server.respond_file ~content_type: "application/pdf" ~fname: path_pdf
