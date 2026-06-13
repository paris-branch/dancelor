open NesUnix
open Dancelor_common
open Model_new
open Search_new

(* FIXME: The following conversion functions are temporary. We will
   save some network by having them happen on the server, but they
   should be pushed into individual controllers in a first place, and
   then even all the way to the respective databases. *)

let to_row env (book : Model.Book.entry) : Book_row.t Lwt.t =
  let user = Environment.user env in
  let%lwt authors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Book.authors' book in
  let authors = List.map Person.to_name authors in
  lwt {
    Book_row.id = Entry.id book;
    name = NEString.to_string @@ Model.Book.name' book;
    date = Model.Book.date' book;
    authors: Person_name.t list;
    permission = Option.get @@ Permission.With_reason.can_get_private user book;
  }

let to_view env (book : Model.Book.entry) : Book_view.t Lwt.t =
  let user = Environment.user env in
  let%lwt authors = Lwt_list.map_s (Option.get <%> Model.Person.get) @@ Model.Book.authors' book in
  let authors = List.map Person.to_name authors in
  let%lwt sources = Lwt_list.map_s (Option.get <%> Model.Source.get) @@ Model.Book.sources' book in
  let sources = List.map Source.to_name sources in
  lwt {
    Book_view.id = Entry.id book;
    name = NEString.to_string @@ Model.Book.name' book;
    date = Model.Book.date' book;
    authors;
    contents = Model.Book.contents' book;
    remark = Option.map NEString.to_string @@ Model.Book.remark' book;
    sources;
    scddb_id = Model.Book.scddb_id' book;
    permission = Option.get @@ Permission.With_reason.can_get_private user book;
  }

let get env id =
  match%lwt Database.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get_private env book;%lwt
    lwt book

let get_row env id =
  to_row env =<< get env id

let get_view env id =
  to_view env =<< get env id

(** Returns a hash table containing as many of the ids as possible. *)
let get_rows_table env ids =
  let table = Hashtbl.create 8 in
  Lwt_list.iter_s
    (fun id ->
      let%lwt book = Database.Book.get id in
      Monadise_lwt.lift_1_1
        Option.iter
        (fun book ->
          if%lwt Permission.can_get_private env book then
            Hashtbl.add table id <$> to_row env book
          else
            lwt_unit
        )
        book
    )
    ids;%lwt
  lwt table

let get_rows env ids =
  let%lwt table = get_rows_table env ids in
  lwt @@ List.filter_map (Hashtbl.find_opt table) ids

let create env book access =
  Permission.assert_can_create_private env;%lwt
  Database.Book.create book access

let update env id book access =
  Permission.assert_can_update_private env =<< get env id;%lwt
  Database.Book.update id book access

let delete env id =
  Permission.assert_can_delete_private env =<< get env id;%lwt
  Database.Book.delete id

include Search.Build(struct
  type value = Model.Book.entry
  type filter = (Model.Book.t, Filter.Book.t) Formula_entry.private_

  let get_all env =
    let all = Database.Book.get_all () in
    let stream = (Lwt_stream.filter_s (Permission.can_get_private env) % Lwt_stream.of_list) <$> all in
    Lwt_stream.flip_lwt stream

  let optimise_filter = Text_formula_converter.optimise (Formula_entry.converter_private Filter.Book.converter)
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Formula_entry.accepts_private Model.User.get Filter.Book.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[decreasing (lwt % Model.Book.date') (Option.compare PartialDate.compare);
    increasing (lwt % NEString.to_string % Model.Book.name') String.Sensible.compare;
    ]
end)

let build_pdf env id book_params rendering_params =
  get env id >>= fun book ->
  let%lwt pdf_metadata =
    let title = NEString.to_string @@ Model.Book.name' book in
    let%lwt authors = Model_to_renderer.format_persons_list <$> Lwt_list.map_p (Option.get <%> Model.Person.get) (Model.Book.authors' book) in
    lwt Renderer.{title; authors; subjects = []}
  in
  let%lwt book = Model_to_renderer.book_to_renderer_book' book book_params in
  let%lwt book_pdf_arg = Model_to_renderer.renderer_book_to_renderer_book_pdf_arg book rendering_params pdf_metadata in
  uncurry Job.register_job_and_file <$> Renderer.make_book_pdf book_pdf_arg

let search env slice filter =
  let%lwt result = search env slice filter in
  let%lwt items = Lwt_list.map_s (to_row env) result.items in
  lwt {result with items}

let search'_new env query =
  let user = Environment.user env in
  let%lwt items = Database.Book.search ~user: (Option.map Entry.id user) query in
  lwt {Search_result.total = List.length items; items}

let search_new env slice query =
  let%lwt {total; items} = search'_new env query in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Search_result.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Get_rows -> get_rows env
  | Search_new -> search_new env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Build_pdf -> build_pdf env
