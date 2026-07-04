(** {1 Client Router} *)

open Nes
open Model_builder
open Model_new

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | In_search of string
  | In_set of Core.Set.t Entry.Id.t * int
  | In_book of Book_id.t * int
[@@deriving yojson, variants]

(* For serialisation *)
module Context = struct type t = context [@@deriving yojson] end

(** {2 Endpoints} *)

(* NOTE: The order matters. For instance, `Book_add` appears before `Book`,
   because otherwise `/book/add` will be seen as `Book` with id `add`. *)

type (_, _, _) person =
  | Add : ('w, 'w, Void.t) person
  | Edit : (Person_id.t -> 'w, 'w, Void.t) person
  | View : (Context.t option -> Person_id.t -> 'w, 'w, Void.t) person
[@@deriving madge_wrapped_endpoints]

type (_, _, _) dance =
  | Add : ('w, 'w, Void.t) dance
  | Edit : (Dance_id.t -> 'w, 'w, Void.t) dance
  | View : (Context.t option -> Dance_id.t -> 'w, 'w, Void.t) dance
[@@deriving madge_wrapped_endpoints]

type (_, _, _) source =
  | Add : ('w, 'w, Void.t) source
  | Edit : (Source_id.t -> 'w, 'w, Void.t) source
  | View : (Context.t option -> Source_id.t -> 'w, 'w, Void.t) source
[@@deriving madge_wrapped_endpoints]

type (_, _, _) tune =
  | Add : ('w, 'w, Void.t) tune
  | Edit : (Tune_id.t -> 'w, 'w, Void.t) tune
  | View : (Context.t option -> Tune_id.t -> 'w, 'w, Void.t) tune
[@@deriving madge_wrapped_endpoints]

type (_, _, _) version =
  | Add : (Tune_id.t option -> 'w, 'w, Void.t) version
  | Edit : (Version_id.t -> 'w, 'w, Void.t) version
  | View : (Context.t option -> Version_id.t -> 'w, 'w, Void.t) version
[@@deriving madge_wrapped_endpoints]

type (_, _, _) set =
  | Add : ('w, 'w, Void.t) set
  | Edit : (Set_id.t -> 'w, 'w, Void.t) set
  | View : (Context.t option -> Set_id.t -> 'w, 'w, Void.t) set
[@@deriving madge_wrapped_endpoints]

type (_, _, _) book =
  | Add : ('w, 'w, Void.t) book
  | Edit : (Book_id.t -> 'w, 'w, Void.t) book
  | View : (Context.t option -> Book_id.t -> 'w, 'w, Void.t) book
[@@deriving madge_wrapped_endpoints]

type (_, _, _) user =
  | Create : ('w, 'w, Void.t) user
  | Prepare_reset_password : ('w, 'w, Void.t) user
  | Password_reset : (Username.t -> Core.User.Password_reset_token_clear.t -> 'w, 'w, Void.t) user
[@@deriving madge_wrapped_endpoints]

(* FIXME: Make routes independent, for instance with paths /add/book instead of
   /book/add (or new/edit/create), for /book/view/<id> vs /book/add. *)
type (_, _, _) t =
  | Index : ('w, 'w, Void.t) t
  | Explore : (string -> int -> 'w, 'w, Void.t) t
  | Any : (unit Entry.Id.t -> 'w, 'w, Void.t) t
  (* lifted endpoints *)
  | Person : ('a, 'w, 'r) person -> ('a, 'w, 'r) t
  | Dance : ('a, 'w, 'r) dance -> ('a, 'w, 'r) t
  | Source : ('a, 'w, 'r) source -> ('a, 'w, 'r) t
  | Tune : ('a, 'w, 'r) tune -> ('a, 'w, 'r) t
  | Version : ('a, 'w, 'r) version -> ('a, 'w, 'r) t
  | Set : ('a, 'w, 'r) set -> ('a, 'w, 'r) t
  | Book : ('a, 'w, 'r) book -> ('a, 'w, 'r) t
  | User : ('a, 'w, 'r) user -> ('a, 'w, 'r) t
[@@deriving madge_wrapped_endpoints]

(** {2 Routes} *)

open Madge

let route_person : type a w r. (a, w, r) person -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Person_id) @@ void ()
    | Add -> literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Person_id) @@ void ()

let route_dance : type a w r. (a, w, r) dance -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Dance_id) @@ void ()
    | Add -> literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Dance_id) @@ void ()

let route_source : type a w r. (a, w, r) source -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Source_id) @@ void ()
    | Add -> literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Source_id) @@ void ()

let route_tune : type a w r. (a, w, r) tune -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Tune_id) @@ void ()
    | Add -> literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Tune_id) @@ void ()

let route_version : type a w r. (a, w, r) version -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Version_id) @@ void ()
    | Add -> query_json_opt "tune" (module Entry.Id.J(Core.Tune)) @@ literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Version_id) @@ void ()

let route_set : type a w r. (a, w, r) set -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Set_id) @@ void ()
    | Add -> literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Set_id) @@ void ()

let route_book : type a w r. (a, w, r) book -> (a, w, r) route =
  let open Route in
  function
    | View -> query_json_opt "context" (module Context) @@ variable (module Book_id) @@ void ()
    | Add -> literal "add" @@ void ()
    | Edit -> literal "edit" @@ variable (module Book_id) @@ void ()

let route_user : type a w r. (a, w, r) user -> (a, w, r) route =
  let open Route in
  function
    | Create -> literal "create" @@ void ()
    | Prepare_reset_password -> literal "prepare-reset-password" @@ void ()
    | Password_reset -> literal "reset-password" @@ query_json "username" (module Username) @@ query_json "token" (module Core.User.Password_reset_token_clear) @@ void ()

(* FIXME: Factorise adding the model prefixes. *)
let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Index -> void ()
    | Explore -> literal "explore" @@ query_str_def "q" (module SString) ~def: "" @@ query_json_def "page" (module JInt) ~def: 1 @@ void ()
    | Any -> variable (module Entry.Id.S(SUnit)) @@ void ()
    | Person page -> literal "person" @@ route_person page
    | Dance page -> literal "dance" @@ route_dance page
    | Source page -> literal "source" @@ route_source page
    | Tune page -> literal "tune" @@ route_tune page
    | Version page -> literal "version" @@ route_version page
    | Set page -> literal "set" @@ route_set page
    | Book page -> literal "book" @@ route_book page
    | User page -> literal "user" @@ route_user page

let href : type a r. (a, Uri.t, r) t -> a = fun page ->
  with_request (route page) @@ fun (module _) request ->
  assert (Request.meth request = GET);
  Request.uri request

let href_book ?context book = href (Book View) context book
let href_dance ?context dance = href (Dance View) context dance
let href_person ?context person = href (Person View) context person
let href_source ?context source = href (Source View) context source
let href_set ?context set = href (Set View) context set
let href_tune ?context tune = href (Tune View) context tune
let href_version ?context version = href (Version View) context version

let href_any_full ?context any =
  let open Core.Any in
  match any with
  | Version version -> href_version ?context (Entry.id version)
  | Set set -> href_set ?context (Entry.id set)
  | Person person -> href_person ?context (Entry.id person)
  | Source source -> href_source ?context (Entry.id source)
  | Dance dance -> href_dance ?context (Entry.id dance)
  | Book book -> href_book ?context (Entry.id book)
  | Tune tune -> href_tune ?context (Entry.id tune)
  | User _ -> Uri.of_string "/" (* FIXME: user visualisation page *)

let href_any_full_new ?context (any : Any_id.t) =
  match any with
  | Version version -> href_version ?context version
  | Set set -> href_set ?context set
  | Person person -> href_person ?context person
  | Source source -> href_source ?context source
  | Dance dance -> href_dance ?context dance
  | Book book -> href_book ?context book
  | Tune tune -> href_tune ?context tune
  | User _user -> Uri.of_string "/" (* FIXME: user visualisation page *)

(** Function that consumes all endpoints and returns nothing. It is meant to be
    used in the catch-all case of a pattern matching. *)
let consume : type a w r. return: w -> (a, w, r) t -> a = fun ~return: value endpoint ->
  match endpoint with
  | Index -> value
  | Any -> const value
  | Book View -> const2 value
  | Book Add -> value
  | Book Edit -> const value
  | Dance View -> const2 value
  | Dance Add -> value
  | Dance Edit -> const value
  | Person View -> const2 value
  | Person Add -> value
  | Person Edit -> const value
  | Set View -> const2 value
  | Set Add -> value
  | Set Edit -> const value
  | Source View -> const2 value
  | Source Add -> value
  | Source Edit -> const value
  | Tune View -> const2 value
  | Tune Add -> value
  | Tune Edit -> const value
  | Version View -> const2 value
  | Version Add -> const value
  | Version Edit -> const value
  | Explore -> const2 value
  | User Create -> value
  | User Prepare_reset_password -> value
  | User Password_reset -> const2 value

module Make_describe (Model : Model_builder.S) = struct
  let describe = fun uri ->
    let describe : type a r. (a, (string * string) option Lwt.t, r) t -> a = function
      | Any -> (fun id -> lwt_some ("any", Entry.Id.to_string id))
      | Version View ->
        (fun _ id ->
          let%lwt name = NEString.to_string <$> (Model.Version.one_name' % Option.get =<< Model.Version.get id) in
          lwt_some ("version", name)
        )
      | Tune View ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Tune.one_name' % Option.get <$> Model.Tune.get id in
          lwt_some ("tune", name)
        )
      | Set View ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Set.name' % Option.get <$> Model.Set.get id in
          lwt_some ("set", name)
        )
      | Book View ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Book.name' % Option.get <$> Model.Book.get id in
          lwt_some ("book", name)
        )
      | Dance View ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Dance.one_name' % Option.get <$> Model.Dance.get id in
          lwt_some ("dance", name)
        )
      | Person View ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Person.name' % Option.get <$> Model.Person.get id in
          lwt_some ("person", name)
        )
      | Source View ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Source.name' % Option.get <$> Model.Source.get id in
          lwt_some ("source", name)
        )
      | endpoint -> consume endpoint ~return: lwt_none
    in
    let madge_match_apply_all : (string * string) option Lwt.t wrapped' list -> (unit -> (string * string) option Lwt.t) option =
      List.find_map @@ fun (W' page) ->
      Madge.apply' (route page) (fun () -> describe page) (Request.make ~meth: GET ~uri ~body: "")
    in
    match madge_match_apply_all @@ all' () with
    | Some page -> page ()
    | None -> (* 404 page *) lwt_none
end
