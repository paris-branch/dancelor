open NesUnix
open Common

module Log = (val Logger.create "controller.book.pdf": Logs.LOG)

let cache : ([`Pdf] * Model.Book.t Entry.t * Model.BookParameters.t * string, string Lwt.t) StorageCache.t =
  StorageCache.create ()

let populate_cache () =
  ControllerCache.populate ~cache ~type_: "book" ~ext: ".pdf" ~pp_ext: "pdf"

let render parameters book =
  let%lwt body = Model.Book.lilypond_contents_cache_key book in
  StorageCache.use ~cache ~key: (`Pdf, book, parameters, body) @@ fun hash ->
  let%lwt lilypond = Ly.render parameters book in
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
  Lwt.return path_pdf

let get env parameters book =
  let%lwt book = Model.Book.get book in
  Permission.assert_can_get env book;%lwt
  let%lwt path_pdf = render parameters book in
  Madge_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
