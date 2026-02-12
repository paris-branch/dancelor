open NesUnix
open Common

module Log = (val Logger.create "controller.user": Logs.LOG)

let get env id =
  match Database.User.get id with
  | None -> Permission.reject_can_get ()
  | Some user ->
    Permission.assert_can_get_public env user;%lwt
    lwt user

let status = lwt % Environment.user

let sign_in env username password remember_me =
  Log.info (fun m -> m "Attempt to sign in with username `%s`." username);
  match Environment.user env with
  | Some _user ->
    Log.info (fun m -> m "Rejecting because already signed in.");
    lwt_none
  | None ->
    match%lwt Database.User.get_from_username @@ NEString.of_string_exn username with
    | None ->
      Log.info (fun m -> m "Rejecting because of wrong username.");
      lwt_none
    | Some user ->
      match Model.User.password' user with
      | None ->
        Log.info (fun m -> m "Rejecting because user has no password.");
        lwt_none
      | Some hashedPassword when not @@ HashedSecret.is ~clear: password hashedPassword ->
        Log.info (fun m -> m "Rejecting because passwords do not match.");
        lwt_none
      | Some _ ->
        Environment.sign_in env user ~remember_me;%lwt
        Log.info (fun m -> m "Accepted sign in for %a." Environment.pp env);
        lwt_some user

let sign_out env =
  match Environment.user env with
  | None -> lwt_unit
  | Some user -> Environment.sign_out env user

let create env user =
  Permission.assert_can_administrate env @@ fun _admin ->
  (* FIXME: A module for usernames that reject malformed ones *)
  (* match Entry.Id.check username with *)
  (* | false -> Madge_server.shortcut_bad_request "The username does not have the right shape." *)
  (* | true -> *)
  let token = Model.User.Password_reset_token_clear.make () in
  (* NOTE: We should use Password_reset_token_hashed.make here, but HashedSecret.make
     is only available on the server side (NesHashedSecretUnix), not in common code. *)
  let hashed_token = (
    Model.User.Password_reset_token_hashed.inject @@ HashedSecret.make ~clear: (Model.User.Password_reset_token_clear.project token),
    Datetime.make_in_the_future (float_of_int @@ 3 * 24 * 3600)
  )
  in
  let%lwt user = Model.User.update user ~password_reset_token: (const @@ Some hashed_token) in
  let%lwt user = Database.User.create user Entry.Access.Public in
  lwt (user, token)

let prepare_reset_password env username =
  Permission.assert_can_administrate env @@ fun _admin ->
  Log.info (fun m -> m "Preparing password reset for user `%s`." username);
  match%lwt Database.User.get_from_username @@ NEString.of_string_exn username with
  | None ->
    Log.info (fun m -> m "Rejecting because username not found.");
    Madge_server.shortcut_bad_request "User not found."
  | Some user ->
    let token = Model.User.Password_reset_token_clear.make () in
    (* NOTE: We should use Password_reset_token_hashed.make here, but HashedSecret.make
       is only available on the server side (NesHashedSecretUnix), not in common code. *)
    let hashed_token = (
      Model.User.Password_reset_token_hashed.inject @@ HashedSecret.make ~clear: (Model.User.Password_reset_token_clear.project token),
      Datetime.make_in_the_future (float_of_int @@ 3 * 24 * 3600)
    )
    in
    ignore
    <$> Database.User.update
        (Entry.id user)
        {(Entry.value user) with
          password = None;
          password_reset_token = Some hashed_token;
          remember_me_tokens = Model.User.Remember_me_key.Map.empty;
        }
        Entry.Access.Public;%lwt
    Log.info (fun m -> m "Password reset token generated for user `%s`." username);
    lwt token

let reset_password username token password =
  Log.info (fun m -> m "Attempt to reset password for user `%s`." username);
  (* FIXME: A module for usernames that reject malformed ones *)
  (* match Entry.Id.check username with *)
  (* | false -> *)
  (*   Log.info (fun m -> m "Rejecting because username is not even a id."); *)
  (*   Madge_server.shortcut_bad_request "The username does not have the right shape." *)
  (* | true -> *)
  match%lwt Database.User.get_from_username @@ NEString.of_string_exn @@ username with
  | None ->
    Log.info (fun m -> m "Rejecting because of wrong username.");
    Madge_server.shortcut_forbidden_no_leak ()
  | Some user ->
    match Model.User.password_reset_token' user with
    | None ->
      Log.info (fun m -> m "Rejecting because of lack of token.");
      Madge_server.shortcut_forbidden_no_leak ()
    | Some (_, token_date) when Datetime.(diff (now ())) token_date > 3. *. 24. *. 3600. ->
      Log.info (fun m -> m "Rejecting because token is too old.");
      Madge_server.shortcut_forbidden_no_leak ()
    | Some (hashed_token, _) ->
      (* NOTE: We should be able to compare Password_reset_token_clear with
         Password_reset_token_hashed directly, but we need to project because
         HashedSecret.is is only available on the server side. *)
      if not @@ HashedSecret.is ~clear: (Model.User.Password_reset_token_clear.project token) (Model.User.Password_reset_token_hashed.project hashed_token) then
        (
          Log.info (fun m -> m "Rejecting because tokens do no match.");
          Madge_server.shortcut_forbidden_no_leak ()
        )
      else
        (
          Log.info (fun m -> m "Accepting to reset password.");
          let password = HashedSecret.make ~clear: password in
          ignore
          <$> Database.User.update
              (Entry.id user)
              {(Entry.value user) with
                password = Some password;
                password_reset_token = None;
              }
              Entry.Access.Public
        )

let set_omniscience env value =
  Permission.assert_can_administrate env @@ fun user ->
  ignore
  <$> Database.User.update
      (Entry.id user)
      {(Entry.value user) with
        role = (
          match Model.User.role' user with
          | Administrator {omniscience = _} -> Administrator {omniscience = value}
          | _ -> assert false
        )
      }
      Entry.Access.Public

include Search.Build(struct
  type value = Model.User.entry
  type filter = Filter.User.t

  let get_all env =
    Lwt_stream.filter (Permission.can_get_public env) @@ Lwt_stream.of_seq @@ Database.User.get_all ()

  let optimise_filter = Filter.User.optimise
  let filter_is_empty = (=) Formula.False
  let filter_accepts = Filter.User.accepts
  let score_true = Formula.interpret_true

  let tiebreakers =
    Lwt_list.[increasing (lwt % NEString.to_string % Model.User.username') String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.User.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Status -> status env
  | Sign_in -> sign_in env
  | Sign_out -> sign_out env
  | Create -> create env
  | Prepare_reset_password -> prepare_reset_password env
  | Reset_password -> reset_password
  | Search -> search env
  | Set_omniscience -> set_omniscience env
