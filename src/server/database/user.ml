open NesUnix
open Dancelor_common

module User_sql = User_sql.Sqlgg(Sqlgg_postgresql)
module Password_hashed = Fresh.Make(HashedSecret)
module Password_reset_token_hashed = Fresh.Make(HashedSecret)

module Remember_me_key = struct
  include Fresh.Make(String)
  module Map = struct
    include Map.Make(struct
      type nonrec t = t
      let compare a b = String.compare (project a) (project b)
    end)

    let to_yojson value_to_yojson map =
      `Assoc (List.map (fun (k, v) -> (project k, value_to_yojson v)) (bindings map))

    let of_yojson value_of_yojson = function
      | `Assoc pairs ->
        List.fold_left
          (fun acc (k, v) ->
            Result.bind acc @@ fun map ->
            Result.map (fun v -> add (inject k) v map) (value_of_yojson v)
          )
          (Ok empty)
          pairs
      | _ -> Error "Expected JSON object"
  end
end

module Remember_me_token_clear = Fresh.Make(String)
module Remember_me_token_hashed = Fresh.Make(HashedSecret)

type remember_me_tokens =
(Remember_me_token_hashed.t * Datetime.t) Remember_me_key.Map.t
[@@deriving yojson]

(* NOTE: Do not reorder as that would break serialisation to and deserialisation
   from PostgreSQL. *)
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

type t = {
  username: Username.t;
  password: Password_hashed.t option;
  password_reset_token: (Password_reset_token_hashed.t * Datetime.t) option;
  remember_me_tokens: remember_me_tokens;
  role: role;
  omniscience: bool;
}
[@@deriving fields, make]

type entry = t Entry.public

let id_to_common : t Entry.id -> Entry.User.t Entry.id = Entry.Id.unsafe_coerce
let id_of_common : Entry.User.t Entry.id -> t Entry.id = Entry.Id.unsafe_coerce

let to_common user =
  Entry.User.make ~username: (username user) ~role: (role_to_common (omniscience user) (role user)) ()

let entry_to_common user =
  Entry.unsafe_map_value to_common user

let row_to_user
    ~id
    ~username
    ~password
    ~password_reset_token_hash
    ~password_reset_token_max_date
    ~remember_me_tokens
    ~role
    ~omniscience
    ~created_at
    ~modified_at
  =
  Entry.make
    ~id
    ~meta: (Entry.Meta.make ~created_at ~modified_at ())
    ~access: Entry.Access.Public
    (
      make
        ~username: (Username.of_string_exn username)
        ?password: (Option.map (Password_hashed.inject % HashedSecret.unsafe_of_string) password)
        ?password_reset_token: (
          Option.bind password_reset_token_hash @@ fun password_reset_token_hash ->
          Option.bind password_reset_token_max_date @@ fun password_reset_token_max_date ->
          Some (
            (Password_hashed.inject % HashedSecret.unsafe_of_string) password_reset_token_hash,
            password_reset_token_max_date
          )
        )
        ~remember_me_tokens: (Result.get_ok @@ remember_me_tokens_of_yojson remember_me_tokens)
        ~role: (Option.get @@ role_of_enum @@ Int64.to_int role)
        ~omniscience
        ()
    )

let get id : entry option Lwt.t =
  Connection.with_ @@ fun db ->
  User_sql.Single.get db ~id: (Entry.Id.to_string id) (row_to_user ~id)

let get_all () : entry list Lwt.t =
  Connection.with_ @@ fun db ->
  User_sql.List.get_all db (fun ~id -> row_to_user ~id: (Entry.Id.of_string_exn id))

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
  (* FIXME: the [created_at] and [modified_at] values here will be wrong *)
  lwt @@
  Entry.make ~id ~access: Entry.Access.Public @@
  make
    ~username
    ~role
    ~omniscience
    ~password_reset_token: (password_reset_token_hash, password_reset_token_max_date)
    ~remember_me_tokens: Remember_me_key.Map.empty
    ()

let update id user =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    User_sql.update
      db
      ~id: (Entry.Id.to_string id)
      ~username: (Username.to_string @@ username user)
      ~password: (Option.map (HashedSecret.unsafe_to_string % Password_hashed.project) @@ password user)
      ~password_reset_token_hash: (Option.map (HashedSecret.unsafe_to_string % Password_reset_token_hashed.project % fst) @@ password_reset_token user)
      ~password_reset_token_max_date: (Option.map snd @@ password_reset_token user)
      ~remember_me_tokens: (remember_me_tokens_to_yojson @@ remember_me_tokens user)
      ~role: (Int64.of_int @@ role_to_enum @@ role user)
      ~omniscience: (omniscience user)
  in
  (* FIXME: the [created_at] and [modified_at] values here will be wrong *)
  lwt @@ Entry.make ~id ~access: Entry.Access.Public user

let delete id =
  let%lwt _ =
    Connection.with_ @@ fun db ->
    User_sql.delete db ~id: (Entry.Id.to_string id)
  in
  lwt_unit

let get_from_username target_username =
  (* FIXME: just write an efficient query for this, once the username is its own
     column (with an index). *)
  let%lwt all = get_all () in
  let this = List.filter ((=) target_username % username % Entry.value) all in
  match this with
  | [] -> lwt_none
  | [this] -> lwt_some this
  | _ -> assert false

let set_password_reset_token id password_reset_token_hash password_reset_token_max_date =
  Connection.with_ @@ fun db ->
  ignore
  <$> User_sql.set_password_reset_token
      db
      ~id: (Entry.Id.to_string id)
      ~password_reset_token_hash: (Some (HashedSecret.unsafe_to_string @@ Password_reset_token_hashed.project password_reset_token_hash))
      ~password_reset_token_max_date: (Some password_reset_token_max_date)

let set_password id password =
  Connection.with_ @@ fun db ->
  ignore
  <$> User_sql.set_password
      db
      ~id: (Entry.Id.to_string id)
      ~password: (some @@ HashedSecret.unsafe_to_string @@ Password_hashed.project password)

let find_remember_me_token user key =
  Remember_me_key.Map.find_opt key (Entry.value user).remember_me_tokens

let add_remember_me_token user key hashed_token max_date =
  ignore <$> update (Entry.id user) {(Entry.value user) with remember_me_tokens = Remember_me_key.Map.add key (hashed_token, max_date) (Entry.value user).remember_me_tokens}

let remove_one_remember_me_token user key =
  ignore <$> update (Entry.id user) {(Entry.value user) with remember_me_tokens = Remember_me_key.Map.remove key (Entry.value user).remember_me_tokens}

let remove_all_remember_me_tokens user =
  ignore <$> update (Entry.id user) {(Entry.value user) with remember_me_tokens = Remember_me_key.Map.empty}

let set_omniscience id value =
  Connection.with_ @@ fun db ->
  ignore <$> User_sql.set_omniscience db ~id: (Entry.Id.to_string id) ~omniscience: value
