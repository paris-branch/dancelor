open Nes
open Dancelor_common
open Model_new

module Entry_sql = Entry_sql.Sqlgg(Sqlgg_postgresql)

type type_ = [
  | `Book
  | `Dance
  | `Person
  | `Set
  | `Source
  | `Tune
  | `User
  | `Version
]

type visibility = [
  | `Owners_only
  | `Everyone
  | `Select_viewers
]

type visibility_or_public = [visibility | `Public]

let classify_type : type_ -> [`Public | `Private] = function
  | `Dance | `Person | `Source | `Tune | `User | `Version -> `Public
  | `Set | `Book -> `Private

let get_type db id =
  Entry_sql.get_type db ~id: (Entry.Id.to_string id)

(** Handles only the insertion into the ["entry"] table. In
    particular, this function does not handle the ["entry_viewers"]
    and ["entry_owners"] tables; see {!insert_or_update_private}. *)
let insert_to_entry_table db ~visibility type_ =
  let rec make () =
    let id = Entry.Id.make () in
    match%lwt get_type db id with
    | None ->
      let%lwt _ = Entry_sql.register db ~id: (Entry.Id.to_string id) ~type_ ~visibility in
      lwt @@ Entry.Id.unsafe_coerce id
    | Some _ ->
      make () (* extremely unlikely *)
  in make ()

(** Takes a function [f] that handles inserting/updating to the
    ["entry"] table and handles everything else that has to do with
    private access. *)
let insert_or_update_private db access f =
  let (visibility, viewers) =
    match Entry.Access.Private.visibility access with
    | Owners_only -> (`Owners_only, [])
    | Everyone -> (`Everyone, [])
    | Select_viewers viewers -> (`Select_viewers, NEList.to_list viewers)
  in
  let%lwt id = f visibility in
  ignore <$> Entry_sql.delete_all_viewers db ~entry_id: (Entry.Id.to_string id);%lwt
  Lwt_list.iter_s
    (fun viewer ->
      ignore
      <$> Entry_sql.add_one_viewer
          db
          ~entry_id: (Entry.Id.to_string id)
          ~viewer_id: (Entry.Id.to_string viewer)
    )
    viewers;%lwt
  ignore <$> Entry_sql.delete_all_owners db ~entry_id: (Entry.Id.to_string id);%lwt
  Lwt_list.iter_s
    (fun owner ->
      ignore
      <$> Entry_sql.add_one_owner
          db
          ~entry_id: (Entry.Id.to_string id)
          ~owner_id: (Entry.Id.to_string owner)
    )
    (NEList.to_list @@ Entry.Access.Private.owners access);%lwt
  lwt id

let make_public db type_ =
  assert (classify_type type_ = `Public);
  (* Public objects only need the ["entry"] table in which they have
     no visibility field. *)
  insert_to_entry_table db type_ ~visibility: None

let make_private db type_ access =
  assert (classify_type type_ = `Private);
  insert_or_update_private db access @@ fun visibility ->
  insert_to_entry_table db type_ ~visibility: (Some visibility)

let update_private_access db id access =
  ignore
  <$> insert_or_update_private db access @@ fun visibility ->
    ignore <$> Entry_sql.update_visibility db ~id: (Entry.Id.to_string id) ~visibility: (Some visibility);%lwt
    lwt id

let touch db id =
  ignore <$> Entry_sql.touch db ~id: (Entry.Id.to_string id)

let delete db id =
  let id = Entry.Id.to_string id in
  ignore <$> Entry_sql.delete_all_owners db ~entry_id: id;%lwt
  ignore <$> Entry_sql.delete_all_viewers db ~entry_id: id;%lwt
  ignore <$> Entry_sql.delete db ~id

let get_newest ~user ~limit =
  assert (limit <= 1000);
  Connection.with_ @@ fun db ->
  (* FIXME: some gymnastics just because users aren't handled so well yet *)
  let%lwt newest =
    Entry_sql.List.get_newest db ~user_id: (Option.fold user ~some: Entry.Id.to_string ~none: "") ~limit: (Int64.of_int limit) (fun ~id ~type_ ->
      match type_ with
      | `Book -> some @@ Any_id.Book (Entry.Id.of_string_exn id)
      | `Dance -> some @@ Any_id.Dance (Entry.Id.of_string_exn id)
      | `Person -> some @@ Any_id.Person (Entry.Id.of_string_exn id)
      | `Set -> some @@ Any_id.Set (Entry.Id.of_string_exn id)
      | `Source -> some @@ Any_id.Source (Entry.Id.of_string_exn id)
      | `Tune -> some @@ Any_id.Tune (Entry.Id.of_string_exn id)
      | `User -> None (* FIXME: we should handle users too *)
      | `Version -> some @@ Any_id.Version (Entry.Id.of_string_exn id)
    )
  in
  lwt @@ List.filter_map Fun.id newest
