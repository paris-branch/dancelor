open Nes
open Dancelor_common
open Model_new
open Search_new

include Shared.Make_private(struct
  type id = Set_id.t
  type row = Set_row.t
  type view = Set_view.t
  type query = Set_query.t
  include Database.Set
end)

(* Legacy *)

let get env id =
  match%lwt Database.Set.get id with
  | None -> Permission.reject_can_get ()
  | Some set ->
    Permission.assert_can_get_private env set;%lwt
    lwt set

let create env set access =
  Permission.assert_can_create_private env;%lwt
  Database.Set.create set access

let update env id set access =
  let%lwt entry = get env id in
  Permission.assert_can_update_private env entry;%lwt
  Database.Set.update id set access

let delete env id =
  Permission.assert_can_delete_private env =<< get env id;%lwt
  Database.Set.delete id

let build_pdf env id set_params rendering_params =
  get env id >>= fun set ->
  let%lwt pdf_metadata =
    let title =
      NEString.to_string @@
        Option.value (Model.Set_parameters.display_name set_params) ~default: (Model.Set.name' set)
    in
    let%lwt authors = Model_to_renderer.format_persons_list <$> Lwt_list.map_p (Option.get <%> Model.Person.get) (Model.Set.conceptors' set) in
    let subjects =
      match Kind.Dance.to_simple @@ Model.Set.kind' set with
      | None -> ["Medley"]
      | Some (n, bars, base) -> [Kind.Base.to_long_string ~capitalised: true base; spf "%dx%d" n bars]
    in
    lwt Renderer.{title; authors; subjects}
  in
  let%lwt set = Model_to_renderer.set_to_renderer_set' (Entry.id set) set_params in
  let%lwt book_pdf_arg = Model_to_renderer.renderer_set_to_renderer_book_pdf_arg set rendering_params pdf_metadata in
  uncurry Job.register_job_and_file <$> Renderer.make_book_pdf book_pdf_arg

(* Dispatch *)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Set.t -> a = fun env endpoint ->
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
