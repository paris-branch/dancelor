(** {1 Client Router} *)

open Nes
open ModelBuilder

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | InSearch of string
  | InSet of Core.Set.t Entry.Id.t * int
  | InBook of Core.Book.t Entry.Id.t * int
[@@deriving yojson, variants]

(* For serialisation *)
module Context = struct type t = context [@@deriving yojson] end

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [Any.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [* for.Core all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

(* NOTE: The order matters. For instance, `BookAdd` appears before `Book`,
   because otherwise `/book/add` will be seen as `Book` with id `add`. *)
(* FIXME: Make routes independent, for instance with paths /add/book instead of
   /book/add (or new/edit/create), for /book/view/<id> vs /book/add. *)
type (_, _, _) t =
  | Any : ((unit Entry.Id.t -> 'w), 'w, Void.t) t
  | BookAdd : ('w, 'w, Void.t) t
  | BookEdit : ((Core.Book.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Book : ((Context.t option -> Core.Book.t Entry.Id.t -> 'w), 'w, Void.t) t
  | DanceAdd : ('w, 'w, Void.t) t
  | DanceEdit : ((Core.Dance.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Dance : ((Context.t option -> Core.Dance.t Entry.Id.t -> 'w), 'w, Void.t) t
  | PersonAdd : ('w, 'w, Void.t) t
  | PersonEdit : ((Core.Person.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Person : ((Context.t option -> Core.Person.t Entry.Id.t -> 'w), 'w, Void.t) t
  | SetAdd : ('w, 'w, Void.t) t
  | SetEdit : ((Core.Set.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Set : ((Context.t option -> Core.Set.t Entry.Id.t -> 'w), 'w, Void.t) t
  | SourceAdd : ('w, 'w, Void.t) t
  | SourceEdit : ((Core.Source.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Source : ((Context.t option -> Core.Source.t Entry.Id.t -> 'w), 'w, Void.t) t
  | TuneAdd : ('w, 'w, Void.t) t
  | TuneEdit : ((Core.Tune.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Tune : ((Context.t option -> Core.Tune.t Entry.Id.t -> 'w), 'w, Void.t) t
  | VersionAdd : ('w, 'w, Void.t) t
  | VersionEdit : ((Core.Version.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Version : ((Context.t option -> Core.Version.t Entry.Id.t -> 'w), 'w, Void.t) t
  | Index : ('w, 'w, Void.t) t
  | Explore : ((string option -> 'w), 'w, Void.t) t
  | UserCreate : ('w, 'w, Void.t) t
  | UserPasswordReset : ((string -> string -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

open Madge

(* FIXME: Factorise adding the model prefixes. *)
let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Any -> variable (module Entry.Id.S(SUnit)) @@ void ()
    | Book -> literal "book" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Book)) @@ void ()
    | BookAdd -> literal "book" @@ literal "add" @@ void ()
    | BookEdit -> literal "book" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Book)) @@ void ()
    | Dance -> literal "dance" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Dance)) @@ void ()
    | DanceAdd -> literal "dance" @@ literal "add" @@ void ()
    | DanceEdit -> literal "dance" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Dance)) @@ void ()
    | Person -> literal "person" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Person)) @@ void ()
    | PersonAdd -> literal "person" @@ literal "add" @@ void ()
    | PersonEdit -> literal "person" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Person)) @@ void ()
    | Set -> literal "set" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Set)) @@ void ()
    | SetAdd -> literal "set" @@ literal "add" @@ void ()
    | SetEdit -> literal "set" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Set)) @@ void ()
    | Source -> literal "source" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Source)) @@ void ()
    | SourceAdd -> literal "source" @@ literal "add" @@ void ()
    | SourceEdit -> literal "source" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Source)) @@ void ()
    | Tune -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Tune)) @@ void ()
    | TuneAdd -> literal "tune" @@ literal "add" @@ void ()
    | TuneEdit -> literal "tune" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Tune)) @@ void ()
    | Version -> literal "version" @@ query_opt "context" (module Context) @@ variable (module Entry.Id.S(Core.Version)) @@ void ()
    | VersionAdd -> literal "version" @@ literal "add" @@ void ()
    | VersionEdit -> literal "version" @@ literal "edit" @@ variable (module Entry.Id.S(Core.Version)) @@ void ()
    | Index -> void ()
    | Explore -> literal "explore" @@ query_opt "q" (module JString) @@ void ()
    | UserCreate -> literal "user" @@ literal "create" @@ void ()
    | UserPasswordReset -> literal "user" @@ literal "reset-password" @@ query "username" (module JString) @@ query "token" (module JString) @@ void ()

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
  | User _ -> assert false (* FIXME: user visualisation page *)

module MakeDescribe (Model : ModelBuilder.S) = struct
  let describe = fun uri ->
    let describe : type a r. (a, (string * string) option Lwt.t, r) t -> a = function
      | Index -> lwt_none
      | Explore -> const lwt_none
      | VersionAdd -> lwt_none
      | VersionEdit -> const lwt_none
      | TuneAdd -> lwt_none
      | TuneEdit -> const lwt_none
      | SetAdd -> lwt_none
      | SetEdit -> const lwt_none
      | BookAdd -> lwt_none
      | BookEdit -> const lwt_none
      | PersonAdd -> lwt_none
      | PersonEdit -> const lwt_none
      | SourceAdd -> lwt_none
      | SourceEdit -> const lwt_none
      | DanceAdd -> lwt_none
      | DanceEdit -> const lwt_none
      | UserCreate -> lwt_none
      | UserPasswordReset -> const2 lwt_none
      | Any -> (fun id -> lwt_some ("any", Entry.Id.to_string id))
      | Version ->
        (fun _ id ->
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
    in
    let madge_match_apply_all : (string * string) option Lwt.t wrapped' list -> (unit -> (string * string) option Lwt.t) option =
      List.map_first_some @@ fun (W' page) ->
      Madge.apply' (route page) (fun () -> describe page) {meth = GET; uri; body = ""}
    in
    match madge_match_apply_all @@ all' () with
    | Some page -> page ()
    | None -> (* FIXME: 404 page *) assert false
end
