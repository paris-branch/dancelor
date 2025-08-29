open NesUnix
open Common

let get_pdf env id _slug book_params rendering_params =
  match%lwt Model.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get env book;%lwt
    let%lwt fname =
      let%lwt book = ModelToRenderer.book_to_renderer_book' book book_params in
      let%lwt book_pdf_arg = ModelToRenderer.renderer_book_to_renderer_book_pdf_arg book rendering_params in
      Renderer.make_book_pdf book_pdf_arg
    in
    Madge_server.respond_file ~content_type: "application/pdf" ~fname

let get env id =
  match%lwt Database.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get env book;%lwt
    lwt book

let create env book =
  Permission.assert_can_create env;%lwt
  Database.Book.create book

let update env id book =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Book.update id book

include Search.Build(struct
  type value = Model.Book.t Entry.t
  type filter = Filter.Book.t

  let get_all env =
    List.filter (Permission.can_get env)
    <$> Database.Book.get_all ()

  let filter_accepts = Filter.Book.accepts

  let tiebreakers =
    Lwt_list.[decreasing (lwt % Model.Book.date') (Option.compare PartialDate.compare);
    increasing (lwt % NEString.to_string % Model.Book.title') String.Sensible.compare;
    increasing (lwt % NEString.opt_to_string % Model.Book.subtitle') String.Sensible.compare;
    ]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Pdf -> get_pdf env
