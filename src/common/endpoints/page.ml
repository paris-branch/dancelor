(** {1 Client Router} *)

open Nes
open ModelBuilder

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | InSearch of string
  | InSet of Core.Set.t Slug.t * int
  | InBook of Core.Book.t Slug.t * int
[@@deriving yojson, variants]

let inSet' = inSet % Slug.unsafe_of_string
let inBook' = inBook % Slug.unsafe_of_string

(* For serialisation *)
module Context = struct type t = context [@@deriving yojson] end

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [Any.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [* for.Core all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

(* NOTE: The order matters. For instance, `BookAdd` appears before `Book`,
   because otherwise `/book/add` will be seen as `Book` with slug `add`. *)
(* FIXME: Make routes independent, for instance with paths /add/book instead of
   /book/add (or new/edit/create), for /book/view/<slug> vs /book/add. *)
type (_, _, _) t =
  | BookAdd : ('w, 'w, Void.t) t
  | BookEdit : ((Core.Book.t Slug.t -> 'w), 'w, Void.t) t
  | Book : ((Context.t option -> Core.Book.t Slug.t -> 'w), 'w, Void.t) t
  | DanceAdd : ('w, 'w, Void.t) t
  | Dance : ((Context.t option -> Core.Dance.t Slug.t -> 'w), 'w, Void.t) t
  | PersonAdd : ('w, 'w, Void.t) t
  | Person : ((Context.t option -> Core.Person.t Slug.t -> 'w), 'w, Void.t) t
  | SetAdd : ('w, 'w, Void.t) t
  | Set : ((Context.t option -> Core.Set.t Slug.t -> 'w), 'w, Void.t) t
  | SourceAdd : ('w, 'w, Void.t) t
  | Source : ((Context.t option -> Core.Source.t Slug.t -> 'w), 'w, Void.t) t
  | TuneAdd : ('w, 'w, Void.t) t
  | Tune : ((Context.t option -> Core.Tune.t Slug.t -> 'w), 'w, Void.t) t
  | VersionAdd : ((Core.Tune.t Slug.t option -> 'w), 'w, Void.t) t
  | Version : ((Context.t option -> Core.Version.t Slug.t -> 'w), 'w, Void.t) t
  | Index : ('w, 'w, Void.t) t
  | Explore : ((string option -> 'w), 'w, Void.t) t
  | UserCreate : ('w, 'w, Void.t) t
  | UserPasswordReset : ((Core.User.t Slug.t -> string -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

open Madge

(* FIXME: Factorise adding the model prefixes. *)
let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Book -> literal "book" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Book)) @@ void ()
    | BookAdd -> literal "book" @@ literal "add" @@ void ()
    | BookEdit -> literal "book" @@ literal "edit" @@ variable (module SSlug(Core.Book)) @@ void ()
    | Dance -> literal "dance" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Dance)) @@ void ()
    | DanceAdd -> literal "dance" @@ literal "add" @@ void ()
    | Person -> literal "person" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Person)) @@ void ()
    | PersonAdd -> literal "person" @@ literal "add" @@ void ()
    | Set -> literal "set" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Set)) @@ void ()
    | SetAdd -> literal "set" @@ literal "add" @@ void ()
    | Source -> literal "source" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Source)) @@ void ()
    | SourceAdd -> literal "source" @@ literal "add" @@ void ()
    | Tune -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Tune)) @@ void ()
    | TuneAdd -> literal "tune" @@ literal "add" @@ void ()
    | Version -> literal "version" @@ query_opt "context" (module Context) @@ variable (module SSlug(Core.Version)) @@ void ()
    | VersionAdd -> literal "version" @@ literal "add" @@ query_opt "tune" (module JSlug(Core.Tune)) @@ void ()
    | Index -> void ()
    | Explore -> literal "explore" @@ query_opt "q" (module JString) @@ void ()
    | UserCreate -> literal "user" @@ literal "create" @@ void ()
    | UserPasswordReset -> literal "user" @@ literal "reset-password" @@ query "username" (module JSlug(Core.User)) @@ query "token" (module JString) @@ void ()

let href : type a r. (a, string, r) t -> a = fun page ->
  with_request (route page) @@ fun (module _) {meth; uri; _} ->
  assert (meth = GET);
  match Uri.to_string uri with "" -> "/" | uri -> uri

let href_book ?context book = href Book context book
let href_dance ?context dance = href Dance context dance
let href_person ?context person = href Person context person
let href_source ?context source = href Source context source
let href_set ?context set = href Set context set
let href_tune ?context tune = href Tune context tune
let href_version ?context version = href Version context version
let href_versionAdd ?tune () = href VersionAdd tune

let href_any ?context any =
  let open Core.Any in
  match any with
  | Version version -> href_version ?context (Entry.slug version)
  | Set set -> href_set ?context (Entry.slug set)
  | Person person -> href_person ?context (Entry.slug person)
  | Source source -> href_source ?context (Entry.slug source)
  | Dance dance -> href_dance ?context (Entry.slug dance)
  | Book book -> href_book ?context (Entry.slug book)
  | Tune tune -> href_tune ?context (Entry.slug tune)

module MakeDescribe (Model : ModelBuilder.S) = struct
  let describe = fun uri ->
    let describe : type a r. (a, (string * string) option Lwt.t, r) t -> a = function
      | Index -> lwt_none
      | Explore -> const lwt_none
      | VersionAdd -> const lwt_none
      | TuneAdd -> lwt_none
      | SetAdd -> lwt_none
      | BookAdd -> lwt_none
      | BookEdit -> const lwt_none
      | PersonAdd -> lwt_none
      | SourceAdd -> lwt_none
      | DanceAdd -> lwt_none
      | UserCreate -> lwt_none
      | UserPasswordReset -> const2 lwt_none
      | Version ->
        (fun _ slug ->
          let%lwt name = Model.Version.name' =<< Model.Version.get slug in
          lwt_some ("version", name)
        )
      | Tune ->
        (fun _ slug ->
          let%lwt name = Model.Tune.name' <$> Model.Tune.get slug in
          lwt_some ("tune", name)
        )
      | Set ->
        (fun _ slug ->
          let%lwt name = Model.Set.name' <$> Model.Set.get slug in
          lwt_some ("set", name)
        )
      | Book ->
        (fun _ slug ->
          let%lwt title = Model.Book.title' <$> Model.Book.get slug in
          lwt_some ("book", title)
        )
      | Dance ->
        (fun _ slug ->
          let%lwt name = Model.Dance.name' <$> Model.Dance.get slug in
          lwt_some ("dance", name)
        )
      | Person ->
        (fun _ slug ->
          let%lwt name = Model.Person.name' <$> Model.Person.get slug in
          lwt_some ("person", name)
        )
      | Source ->
        (fun _ slug ->
          let%lwt name = Model.Source.name' <$> Model.Source.get slug in
          lwt_some ("source", name)
        )
    in
    let madge_match_apply_all : (string * string) option Lwt.t wrapped' list -> (unit -> (string * string) option Lwt.t) option =
      List.map_first_some @@ fun (W' page) ->
      Madge.apply' (route page) (fun () -> describe page) {meth = GET; uri; body = ""}
    in
    match madge_match_apply_all @@ all' () with
    | Some page -> page ()
    | None -> (* FIXME: 404 page *) assert false
end
