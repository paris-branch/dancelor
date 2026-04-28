open Nes
open Dancelor_common

module User_sql = User_sql.Sqlgg(Sqlgg_postgresql)

type t = Model_builder.Core.User.t
type entry = Model_builder.Core.User.entry

type remember_me_tokens =
(Entry.User.Remember_me_token_hashed.t * Datetime.t) Entry.User.Remember_me_key.Map.t
[@@deriving yojson]

let row_to_user
    ~id
    ~username
    ~password
    ~password_reset_token_hash
    ~password_reset_token_max_date
    ~remember_me_tokens
    ~role
    ~created_at
    ~modified_at
  =
  Entry.make
    ~id
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      Model_builder.Core.User.make
        ~username: (Entry.User.Username.of_string_exn username)
        ?password: (Option.map (Entry.User.Password_hashed.inject % HashedSecret.unsafe_of_string) password)
        ?password_reset_token: (
          Option.bind password_reset_token_hash @@ fun password_reset_token_hash ->
          Option.bind password_reset_token_max_date @@ fun password_reset_token_max_date ->
          Some (
            (Entry.User.Password_hashed.inject % HashedSecret.unsafe_of_string) password_reset_token_hash,
            password_reset_token_max_date
          )
        )
        ~remember_me_tokens: (Result.get_ok @@ remember_me_tokens_of_yojson remember_me_tokens)
        ~role: (Result.get_ok @@ Entry.User.role_of_yojson role)
        ()
    )

let get id : Model_builder.Core.User.entry option Lwt.t =
  Connection.with_ @@ fun db ->
  User_sql.Single.get db ~id: (Entry.Id.to_string id) (row_to_user ~id)

let get_all () =
  Connection.with_ @@ fun db ->
  User_sql.List.get_all db (fun ~id -> row_to_user ~id: (Entry.Id.of_string_exn id))

let create user =
  let%lwt id = Globally_unique_id.make User in
  let user = Entry.make ~id ~access: Entry.Access.Public user in
  Connection.with_ @@ fun db ->
  let%lwt _ =
    User_sql.insert
      db
      ~id: (Entry.Id.to_string id)
      ~username: (Entry.User.Username.to_string @@ Model_builder.Core.User.username' user)
      ~password: (Option.map (HashedSecret.unsafe_to_string % Entry.User.Password_hashed.project) @@ Model_builder.Core.User.password' user)
      ~password_reset_token_hash: (Option.map (HashedSecret.unsafe_to_string % Entry.User.Password_reset_token_hashed.project % fst) @@ Model_builder.Core.User.password_reset_token' user)
      ~password_reset_token_max_date: (Option.map snd @@ Model_builder.Core.User.password_reset_token' user)
      ~remember_me_tokens: (remember_me_tokens_to_yojson @@ Model_builder.Core.User.remember_me_tokens' user)
      ~role: (Entry.User.role_to_yojson @@ Model_builder.Core.User.role' user)
  in
  lwt user

let update id user =
  let user = Entry.make ~id ~access: Entry.Access.Public user in
  let%lwt _ =
    Connection.with_ @@ fun db ->
    User_sql.update
      db
      ~id: (Entry.Id.to_string id)
      ~username: (Entry.User.Username.to_string @@ Model_builder.Core.User.username' user)
      ~password: (Option.map (HashedSecret.unsafe_to_string % Entry.User.Password_hashed.project) @@ Model_builder.Core.User.password' user)
      ~password_reset_token_hash: (Option.map (HashedSecret.unsafe_to_string % Entry.User.Password_reset_token_hashed.project % fst) @@ Model_builder.Core.User.password_reset_token' user)
      ~password_reset_token_max_date: (Option.map snd @@ Model_builder.Core.User.password_reset_token' user)
      ~remember_me_tokens: (remember_me_tokens_to_yojson @@ Model_builder.Core.User.remember_me_tokens' user)
      ~role: (Entry.User.role_to_yojson @@ Model_builder.Core.User.role' user)
  in
  lwt user

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    User_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit

let get_from_username username =
  (* FIXME: just write an efficient query for this, once the username is its own
     column (with an index). *)
  let%lwt all = get_all () in
  let this = List.filter ((=) username % Dancelor_common.Model_builder.Core.User.username % Dancelor_common.Entry.value) all in
  match this with
  | [] -> lwt_none
  | [this] -> lwt_some this
  | _ -> assert false
