(** {1 Client Router} *)

open Nes
open Model_builder

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | In_search of string
  | In_set of Core.Set.t Entry.Id.t * int
  | In_book of Core.Book.t Entry.Id.t * int
[@@deriving yojson, variants]

(* For serialisation *)
module Context = struct type t = context [@@deriving yojson] end

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [Any.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [* for.Core all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

(* NOTE: The order matters. For instance, `Book_add` appears before `Book`,
   because otherwise `/book/add` will be seen as `Book` with id `add`. *)
(* FIXME: Make routes independent, for instance with paths /add/book instead of
   /book/add (or new/edit/create), for /book/view/<id> vs /book/add. *)
type (_, _, _) t =
  | Any : ((unit Entry.Id.t -> 'w), 'w, Void.t) t
  | Book_add : ('w, 'w, Void.t) t
  | Book_edit : ((Core.Book.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Book : ((Context.t option -> Core.Book.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Dance_add : ('w, 'w, Void.t) t
  | Dance_edit : ((Core.Dance.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Dance : ((Context.t option -> Core.Dance.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Person_add : ('w, 'w, Void.t) t
  | Person_edit : ((Core.Person.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Person : ((Context.t option -> Core.Person.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Set_add : ('w, 'w, Void.t) t
  | Set_edit : ((Core.Set.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Set : ((Context.t option -> Core.Set.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Source_add : ('w, 'w, Void.t) t
  | Source_edit : ((Core.Source.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Source : ((Context.t option -> Core.Source.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Tune_add : ('w, 'w, Void.t) t
  | Tune_edit : ((Core.Tune.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Tune : ((Context.t option -> Core.Tune.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Version_add : ('w, 'w, Void.t) t
  | Version_edit : ((Core.Version.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Version : ((Context.t option -> Core.Tune.t Entry.Id.t -> Core.Version.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Index : ('w, 'w, Void.t) t
  | Explore : ((string option -> 'w), 'w, Void.t) t
  | User_create : ('w, 'w, Void.t) t
  | User_prepare_reset_password : ('w, 'w, Void.t) t
  | User_password_reset : ((Core.User.Username.t -> Core.User.Password_reset_token_clear.t -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

open Madge

(* FIXME: Factorise adding the model prefixes. *)
let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Any -> variable (module Entry.Id.S(SUnit)) @@ void ()
    | Book -> literal "book" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Book)) @@ void ()
    | Book_add -> literal "book" @@ literal "add" @@ void ()
    | Book_edit -> literal "book" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Book)) @@ void ()
    | Dance -> literal "dance" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Dance)) @@ void ()
    | Dance_add -> literal "dance" @@ literal "add" @@ void ()
    | Dance_edit -> literal "dance" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Dance)) @@ void ()
    | Person -> literal "person" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Person)) @@ void ()
    | Person_add -> literal "person" @@ literal "add" @@ void ()
    | Person_edit -> literal "person" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Person)) @@ void ()
    | Set -> literal "set" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Set)) @@ void ()
    | Set_add -> literal "set" @@ literal "add" @@ void ()
    | Set_edit -> literal "set" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Set)) @@ void ()
    | Source -> literal "source" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Source)) @@ void ()
    | Source_add -> literal "source" @@ literal "add" @@ void ()
    | Source_edit -> literal "source" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Source)) @@ void ()
    | Tune -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Tune)) @@ void ()
    | Version -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Tune)) @@ variable (module Entry.Id.S(Core.Version)) @@ void ()
    | Tune_add -> literal "tune" @@ literal "add" @@ void ()
    | Tune_edit -> literal "tune" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Tune)) @@ void ()
    | Version_add -> literal "version" @@ literal "add" @@ void ()
    | Version_edit -> literal "version" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Version)) @@ void ()
    | Index -> void ()
    | Explore -> literal "explore" @@ query_opt "q" (module JString) @@ void ()
    | User_create -> literal "user" @@ literal "create" @@ void ()
    | User_prepare_reset_password -> literal "user" @@ literal "prepare-reset-password" @@ void ()
    | User_password_reset -> literal "user" @@ literal "reset-password" @@ query "username" (module Core.User.Username) @@ query "token" (module Core.User.Password_reset_token_clear) @@ void ()

let href : type a r. (a, Uri.t, r) t -> a = fun page ->
  with_request (route page) @@ fun (module _) request ->
  assert (Request.meth request = GET);
  Request.uri request

let href_book ?context book = href Book context book
let href_dance ?context dance = href Dance context dance
let href_person ?context person = href Person context person
let href_source ?context source = href Source context source
let href_set ?context set = href Set context set
let href_tune ?context tune = href Tune context tune

let href_version ?context tune = function
  | None -> href Tune context tune
  | Some version -> href Version context tune version

let href_any_full ?context any =
  let open Core.Any in
  match any with
  | Version version -> href_version ?context (Model_builder.Core.Version.tune' version) (some @@ Entry.id version)
  | Set set -> href_set ?context (Entry.id set)
  | Person person -> href_person ?context (Entry.id person)
  | Source source -> href_source ?context (Entry.id source)
  | Dance dance -> href_dance ?context (Entry.id dance)
  | Book book -> href_book ?context (Entry.id book)
  | Tune tune -> href_tune ?context (Entry.id tune)
  | User _ -> assert false (* FIXME: user visualisation page *)

(** Function that consumes all endpoints and returns nothing. It is meant to be
    used in the catch-all case of a pattern matching. *)
let consume : type a w r. w -> (a, w, r) t -> a = fun value endpoint ->
  match endpoint with
  | Index -> value
  | Any -> const value
  | Book -> const2 value
  | Book_add -> value
  | Book_edit -> const value
  | Dance -> const2 value
  | Dance_add -> value
  | Dance_edit -> const value
  | Person -> const2 value
  | Person_add -> value
  | Person_edit -> const value
  | Set -> const2 value
  | Set_add -> value
  | Set_edit -> const value
  | Source -> const2 value
  | Source_add -> value
  | Source_edit -> const value
  | Tune -> const2 value
  | Version -> (fun _ _ _ -> value)
  | Tune_add -> value
  | Tune_edit -> const value
  | Version_add -> value
  | Version_edit -> const value
  | Explore -> const value
  | User_create -> value
  | User_prepare_reset_password -> value
  | User_password_reset -> const2 value

module Make_describe (Model : Model_builder.S) = struct
  let describe = fun uri ->
    let describe : type a r. (a, (string * string) option Lwt.t, r) t -> a = function
      | Any -> (fun id -> lwt_some ("any", Entry.Id.to_string id))
      | Version ->
        (fun _ _ id ->
          let%lwt name = NEString.to_string <$> (Model.Version.one_name' % Option.get =<< Model.Version.get id) in
          lwt_some ("version", name)
        )
      | Tune ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Tune.one_name' % Option.get <$> Model.Tune.get id in
          lwt_some ("tune", name)
        )
      | Set ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Set.name' % Option.get <$> Model.Set.get id in
          lwt_some ("set", name)
        )
      | Book ->
        (fun _ id ->
          let%lwt title = NEString.to_string % Model.Book.title' % Option.get <$> Model.Book.get id in
          lwt_some ("book", title)
        )
      | Dance ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Dance.one_name' % Option.get <$> Model.Dance.get id in
          lwt_some ("dance", name)
        )
      | Person ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Person.name' % Option.get <$> Model.Person.get id in
          lwt_some ("person", name)
        )
      | Source ->
        (fun _ id ->
          let%lwt name = NEString.to_string % Model.Source.name' % Option.get <$> Model.Source.get id in
          lwt_some ("source", name)
        )
      | endpoint -> consume lwt_none endpoint
    in
    let madge_match_apply_all : (string * string) option Lwt.t wrapped' list -> (unit -> (string * string) option Lwt.t) option =
      List.map_first_some @@ fun (W' page) ->
      Madge.apply' (route page) (fun () -> describe page) (Request.make ~meth: GET ~uri ~body: "")
    in
    match madge_match_apply_all @@ all' () with
    | Some page -> page ()
    | None -> (* 404 page *) lwt_none
end
