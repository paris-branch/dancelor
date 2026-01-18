open Nes
open Common

let can_create () =
  Madge_client.call_exn Endpoints.Api.(route @@ User Can_create)

let can_admin () =
  Madge_client.call_exn Endpoints.Api.(route @@ User Can_admin)

type can_get_private =
  | Everyone (** everyone can see this entry *)
  | Owner (** you can see this entry because you are its owner *)
  | Viewer (** you can see this entry because its owner marked you as a viewer *)
  | Omniscient_administrator (** you can see this entry because you are an administrator with omniscience enabled *)

let (<|>) = Option.(choose ~tie: first)

let can_get_private entry : can_get_private option Lwt.t =
  let%lwt user = Environment.user in
  let access = Entry.access entry in
  let meta_visibility = Entry.Access.Private.meta_visibility access in
  lwt (
    (match meta_visibility with `Everyone -> Some Everyone | _ -> None)
    <|> Option.bind
        user
        (fun user ->
          (if NEList.exists (Entry.Id.equal' (Entry.id user)) (Entry.Access.Private.owners access) then Some Owner else None)
          <|> (match meta_visibility with `Select_viewers viewers when NEList.exists (Entry.Id.equal' (Entry.id user)) viewers -> Some Viewer | _ -> None)
          <|> (if Model.User.is_omniscient_administrator' user then Some Omniscient_administrator else None)
        )
  )
