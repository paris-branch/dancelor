open Nes
module Model = Dancelor_server_model
module Database = Dancelor_server_database

module Pdf = struct
  let render ?parameters set =
    let book =
      Database.Entry.make_dummy @@
      Model.Book.make
        ~title: ""
        ~contents: [InlineSet (Database.Entry.value set, Option.value ~default: Model.SetParameters.none parameters)]
        ()
    in
    let parameters =
      (* FIXME: the fact that we need to transfer this is just wrong. see
         https://github.com/paris-branch/dancelor/issues/250 *)
      Model.BookParameters.make
        ?paper_size: (Option.bind parameters Model.SetParameters.paper_size)
        ()
    in
    Book.Pdf.render book ~parameters

  let get set parameters =
    let%lwt set = Model.Set.get set in
    let%lwt path_pdf = render set ?parameters in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model.SetEndpoints.t -> a = function
  | Get -> Model.Set.get
  | Delete -> (fun slug -> Lwt.bind (Model.Set.get slug) Model.Set.delete)
  | Search -> (fun slice threshold filter -> Model.Set.search ?slice ?threshold filter)
  | Save -> Model.Set.save
  | Pdf -> (fun parameters set -> Pdf.get set parameters)
