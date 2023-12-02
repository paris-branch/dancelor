open Nes
module Model = Dancelor_server_model

module Pdf = struct
  let render ?parameters set =
    let%lwt book =
      Model.Book.make
        ~slug:(Slug.unsafe_coerce @@ Model.Set.slug set)
        ~title:""
        ~contents:[InlineSet (set, Option.value ~default:Model.SetParameters.none parameters)]
        ~modified_at:(Datetime.now ())
        ~created_at:(Datetime.now ())
        ()
    in
    let parameters =
      (* FIXME: the fact that we need to transfer this is just wrong. see
         https://github.com/paris-branch/dancelor/issues/250 *)
      Model.BookParameters.make
        ?paper_size:(Option.bind parameters Model.SetParameters.paper_size)
        ()
    in
    Book.Pdf.render book ~parameters

  let get set parameters =
    let%lwt set = Model.Set.get set in
    let%lwt path_pdf = render set ?parameters in
    Cohttp_lwt_unix.Server.respond_file ~fname:path_pdf ()
end
