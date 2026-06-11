open Nes
open Dancelor_common

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

let check_visibility (visibility : visibility_or_public) (type_ : type_) : visibility option =
  match type_ with
  | `Dance | `Person | `Source | `Tune | `User | `Version ->
    assert (visibility = `Public);
    None
  | `Set | `Book ->
    match visibility with
    | `Public -> assert false
    | `Owners_only | `Everyone | `Select_viewers as visibility -> Some visibility

let get_type db id =
  Entry_sql.get_type db ~id: (Entry.Id.to_string id)

let make_gen db ~visibility type_ =
  let visibility = check_visibility visibility type_ in
  let rec make () =
    let id = Entry.Id.make () in
    match%lwt get_type db id with
    | None ->
      let%lwt _ = Entry_sql.register db ~id: (Entry.Id.to_string id) ~type_ ~visibility in
      lwt @@ Entry.Id.unsafe_coerce id
    | Some _ ->
      make () (* extremely unlikely *)
  in make ()

let touch db id =
  ignore <$> Entry_sql.touch db ~id: (Entry.Id.to_string id)

let delete db id =
  let id = Entry.Id.to_string id in
  ignore <$> Entry_sql.delete_all_owners db ~entry_id: id;%lwt
  ignore <$> Entry_sql.delete_all_viewers db ~entry_id: id;%lwt
  ignore <$> Entry_sql.delete db ~id

let update_private_access db id access =
  let id = Entry.Id.to_string id in
  let (visibility, viewers) =
    match Entry.Access.Private.visibility access with
    | Owners_only -> (`Owners_only, [])
    | Everyone -> (`Everyone, [])
    | Select_viewers viewers -> (`Select_viewers, NEList.to_list viewers)
  in
  ignore <$> Entry_sql.update_visibility db ~id ~visibility: (Some visibility);%lwt
  ignore <$> Entry_sql.delete_all_viewers db ~entry_id: id;%lwt
  Lwt_list.iter_s
    (fun viewer ->
      ignore
      <$> Entry_sql.add_one_viewer
          db
          ~entry_id: id
          ~viewer_id: (Entry.Id.to_string viewer)
    )
    viewers;%lwt
  ignore <$> Entry_sql.delete_all_owners db ~entry_id: id;%lwt
  Lwt_list.iter_s
    (fun owner ->
      ignore
      <$> Entry_sql.add_one_owner
          db
          ~entry_id: id
          ~owner_id: (Entry.Id.to_string owner)
    )
    (NEList.to_list @@ Entry.Access.Private.owners access)

let make_public db type_ = make_gen db type_ ~visibility: `Public

let make_private db type_ access =
  (* NOTE: The first query leaves the DB in an inconsistent state, so
     this should be run in a transactional context. *)
  let%lwt id = make_gen db type_ ~visibility: `Public in
  update_private_access db id access;%lwt
  lwt id
