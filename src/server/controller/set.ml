open Nes
module Model = Dancelor_server_model
module Database = Dancelor_server_database

module Pdf = struct
  let render parameters set =
    let book =
      Database.Entry.make_dummy @@
      Model.Book.make ~title: "" ~contents: [InlineSet (Database.Entry.value set, parameters)] ()
    in
    let parameters =
      (* FIXME: the fact that we need to transfer this is just wrong. see
         https://github.com/paris-branch/dancelor/issues/250 *)
      Model.BookParameters.make ?paper_size: (Model.SetParameters.paper_size parameters) ()
    in
    Book.Pdf.render parameters book

  let get parameters set =
    let%lwt set = Model.Set.get set in
    let%lwt path_pdf = render parameters set in
    Madge_cohttp_lwt_server.shortcut @@ Cohttp_lwt_unix.Server.respond_file ~fname: path_pdf ()
end

let dispatch : type a r. (a, r Lwt.t, r) Dancelor_common_model_endpoints.Set.t -> a = function
  | Get -> Model.Set.get
  | Delete -> (fun slug -> Lwt.bind (Model.Set.get slug) Model.Set.delete)
  | Search -> Model.Set.search
  | Create -> Model.Set.create
  | Update -> Model.Set.update
  | Pdf -> Pdf.get
