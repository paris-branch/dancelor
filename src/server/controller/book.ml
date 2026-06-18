open NesUnix
open Dancelor_common
open Model_new
open Search_new

include Shared.Make_private(struct
  type id = Book_id.t
  type row = Book_row.t
  type view = Book_view.t
  type query = Book_query.t
  include Database.Book
end)

(* Legacy *)

let get env id =
  match%lwt Database.Book.get id with
  | None -> Permission.reject_can_get ()
  | Some book ->
    Permission.assert_can_get_private env book;%lwt
    lwt book

let create env book access =
  Permission.assert_can_create_private env;%lwt
  Database.Book.create book access

let update env id book access =
  Permission.assert_can_update_private env =<< get env id;%lwt
  Database.Book.update id book access

let delete env id =
  Permission.assert_can_delete_private env =<< get env id;%lwt
  Database.Book.delete id

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

(* Dispatch *)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Book.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_row -> get_row env
  | Get_view -> get_view env
  | Get_rows -> get_rows env
  | Search -> search env
  | Create -> create env
  | Update -> update env
  | Delete -> delete env
  | Build_pdf -> build_pdf env
