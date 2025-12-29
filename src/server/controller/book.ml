open NesUnix
open Common

let get env id =
  match Database.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get env book;%lwt
    lwt book

let create env book =
  Permission.assert_can_create env @@ fun user ->
  Database.Book.create ~owner: (Entry.id user) book

let update env id book =
  Permission.assert_can_update env =<< get env id;%lwt
  Database.Book.update id book

let delete env id =
  Permission.assert_can_delete env =<< get env id;%lwt
  Database.Book.delete id

include Search.Build(struct
  type value = Model.Book.t Entry.t
  type filter = Filter.Book.t

  let get_all env =
    Lwt_stream.filter (Permission.can_get env) @@ Lwt_stream.of_seq @@ Database.Book.get_all ()

  let optimise_filter = Filter.Book.optimise
  let filter_is_empty = (=) Formula.False
  let filter_is_full = (=) Formula.True
  let filter_accepts = Filter.Book.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[decreasing (lwt % Model.Book.date') (Option.compare PartialDate.compare);
    increasing (lwt % NEString.to_string % Model.Book.title') String.Sensible.compare;
    ]
end)

let build_pdf env id book_params rendering_params =
  get env id >>= fun book ->
  let%lwt pdf_metadata =
    let title = NEString.to_string @@ Model.Book.title' book in
    let%lwt authors = ModelToRenderer.format_persons_list <$> Model.Book.authors' book in
    lwt Renderer.{title; authors; subjects = []}
  in
  let%lwt book = ModelToRenderer.book_to_renderer_book' book book_params in
  let%lwt book_pdf_arg = ModelToRenderer.renderer_book_to_renderer_book_pdf_arg book rendering_params pdf_metadata in
  lwt @@ uncurry Job.register_job @@ Renderer.make_book_pdf book_pdf_arg

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | BuildPdf -> build_pdf env
