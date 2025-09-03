open NesUnix
open Common

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

let build_pdf env id book_params rendering_params =
  get env id >>= fun book ->
  let%lwt pdf_metadata =
    let title = NEString.to_string @@ Model.Book.title' book in
    let%lwt authors = ModelToRenderer.format_persons_list <$> Model.Book.authors' book in
    lwt Renderer.{title; authors; subjects = []; creator = "FIXME"}
  in
  let%lwt (slug, book) = ModelToRenderer.book_to_renderer_book' book book_params in
  let%lwt book_pdf_arg =
    ModelToRenderer.renderer_book_to_renderer_book_pdf_arg
      slug
      book
      rendering_params
      pdf_metadata
  in
  Job.register_job <$> Renderer.make_book_pdf book_pdf_arg

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | BuildPdf -> build_pdf env
