open Nes
module Model = Dancelor_server_model

module Pdf = struct
  let get set_slug parameters =
    let%lwt set = Model.Set.get set_slug in
    let%lwt book =
      Model.Book.make
        ~slug:(Slug.unsafe_coerce set_slug)
        ~title:""
        ~contents:[Set (set, Option.value ~default:Model.SetParameters.none parameters)]
        ~modified_at:(Datetime.now ())
        ~created_at:(Datetime.now ())
        ()
    in
    let%lwt path_pdf = Book.Pdf.render book in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
