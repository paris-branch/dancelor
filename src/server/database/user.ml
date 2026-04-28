open NesUnix
open Dancelor_common

module User_sql = User_sql.Sqlgg(Sqlgg_postgresql)
module Password_hashed = Fresh.Make(HashedSecret)
module Password_reset_token_hashed = Fresh.Make(HashedSecret)
module Remember_me_key = Fresh.Make(String)
module Remember_me_token_clear = Fresh.Make(String)
module Remember_me_token_hashed = Fresh.Make(HashedSecret)

type t = Entry.User.t
type entry = t Entry.public

(* NOTE: Do not reorder as that would break serialisation to and deserialisation
   from PostgreSQL. *)
(* FIXME: We should just have a proper enum in PostgreSQL... *)
type role =
  | Normal_user
  | Maintainer
  | Administrator
[@@deriving enum]

let role_to_common omniscience = function
  | Normal_user -> Entry.User.Normal_user
  | Maintainer -> Maintainer
  | Administrator -> Administrator {omniscience}

let role_of_common = function
  | Entry.User.Normal_user -> (Normal_user, false)
  | Maintainer -> (Maintainer, false)
  | Administrator {omniscience} -> (Administrator, omniscience)

let row_to_user
    ~id
    ~username
    ~role
    ~omniscience
    ~created_at
    ~modified_at
  =
  Entry.make
    ~id: (Entry.Id.of_string_exn id)
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Entry.User.make
        ~username: (Username.of_string_exn username)
        ~role: (role_to_common omniscience @@ Option.get @@ role_of_enum @@ Int64.to_int role)
        ()
    )

let get id : entry option Lwt.t =
  let id = Entry.Id.to_string id in
  Connection.with_ @@ fun db ->
  User_sql.Single.get db ~id (row_to_user ~id)

let get_from_username username =
  let username = Username.to_string username in
  Connection.with_ @@ fun db ->
  User_sql.Single.get_from_username db ~username (row_to_user ~username)

let get_all () : entry list Lwt.t =
  Connection.with_ @@ fun db ->
  User_sql.List.get_all db row_to_user

let get_password_from_username username =
  let username = Username.to_string username in
  Connection.with_ @@ fun db ->
  Option.join
  <$> User_sql.Single.get_password_from_username db ~username (fun ~password ->
      Option.map (Password_hashed.inject % HashedSecret.unsafe_of_string) password
    )

let get_password_reset_token_from_username username =
  let username = Username.to_string username in
  Connection.with_ @@ fun db ->
  Option.join
  <$> User_sql.Single.get_password_reset_token_from_username db ~username (fun ~password_reset_token_hash ~password_reset_token_max_date ->
      Option.bind password_reset_token_hash @@ fun password_reset_token_hash ->
      Option.bind password_reset_token_max_date @@ fun password_reset_token_max_date ->
      Some (
        (Password_reset_token_hashed.inject @@ HashedSecret.unsafe_of_string password_reset_token_hash),
        password_reset_token_max_date
      )
    )

let create ~username ~role ~password_reset_token_hash ~password_reset_token_max_date =
  let (role, omniscience) = role_of_common role in
  let%lwt id = Globally_unique_id.make User in
  Connection.with_ @@ fun db ->
  let%lwt _ =
    User_sql.create
      db
      ~id: (Entry.Id.to_string id)
      ~username: (Username.to_string username)
      ~role: (Int64.of_int @@ role_to_enum role)
      ~omniscience
      ~password_reset_token_hash: (some @@ HashedSecret.unsafe_to_string @@ Password_reset_token_hashed.project password_reset_token_hash)
      ~password_reset_token_max_date: (Some password_reset_token_max_date)
  in
  lwt id

let remove_all_remember_me_tokens user_id =
  Connection.with_ @@ fun db ->
  (* FIXME: an index covering user_id *)
  ignore
  <$> User_sql.remove_all_remember_me_tokens db ~user_id: (Entry.Id.to_string user_id)

let remove_one_remember_me_token user_id key =
  Connection.with_ @@ fun db ->
  (* FIXME: an index covering (user_id, key) *)
  ignore
  <$> User_sql.remove_one_remember_me_token
      db
      ~user_id: (Entry.Id.to_string user_id)
      ~key: (Remember_me_key.project key)

let set_password_reset_token id password_reset_token_hash password_reset_token_max_date =
  Connection.with_ @@ fun db ->
  ignore <$> remove_all_remember_me_tokens id;%lwt
  ignore
  <$> User_sql.set_password_reset_token
      db
      ~id: (Entry.Id.to_string id)
      ~password_reset_token_hash: (Some (HashedSecret.unsafe_to_string @@ Password_reset_token_hashed.project password_reset_token_hash))
      ~password_reset_token_max_date: (Some password_reset_token_max_date)

let set_password id password =
  Connection.with_ @@ fun db ->
  ignore <$> remove_all_remember_me_tokens id;%lwt
  ignore
  <$> User_sql.set_password
      db
      ~id: (Entry.Id.to_string id)
      ~password: (some @@ HashedSecret.unsafe_to_string @@ Password_hashed.project password)

let find_remember_me_token user_id key =
  Connection.with_ @@ fun db ->
  match%lwt User_sql.List.find_remember_me_token
    db
    ~user_id: (Entry.Id.to_string user_id)
    ~key: (Remember_me_key.project key)
    (fun ~hash ~max_date -> (Remember_me_token_hashed.inject @@ HashedSecret.unsafe_of_string hash, max_date)) with
  | [] -> lwt_none
  | [x] -> lwt_some x
  | _ -> assert false

let add_remember_me_token user_id key hash max_date =
  Connection.with_ @@ fun db ->
  ignore
  <$> User_sql.add_remember_me_token
      db
      ~user_id: (Entry.Id.to_string user_id)
      ~key: (Remember_me_key.project key)
      ~hash: (HashedSecret.unsafe_to_string @@ Remember_me_token_hashed.project hash)
      ~max_date

let set_omniscience id value =
  Connection.with_ @@ fun db ->
  ignore <$> User_sql.set_omniscience db ~id: (Entry.Id.to_string id) ~omniscience: value
