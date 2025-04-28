(** {1 Client Router} *)

open Nes
open ModelBuilder

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | InSearch of string
  | InSet of Set.t Slug.t * int
  | InBook of Book.t Slug.t * int
[@@deriving yojson, variants]

let inSet' = inSet % Slug.unsafe_of_string
let inBook' = inBook % Slug.unsafe_of_string

(* For serialisation *)
module Context = struct type t = context [@@deriving yojson] end

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [Any.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [* for.Core all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

type (_, _, _) t =
  | Book : ((Context.t option -> Book.t Slug.t -> 'w), 'w, Void.t) t
  | BookAdd : ('w, 'w, Void.t) t
  | BookEdit : ((Book.t Slug.t -> 'w), 'w, Void.t) t
  | Dance : ((Context.t option -> Dance.t Slug.t -> 'w), 'w, Void.t) t
  | DanceAdd : ('w, 'w, Void.t) t
  | Person : ((Context.t option -> Person.t Slug.t -> 'w), 'w, Void.t) t
  | PersonAdd : ('w, 'w, Void.t) t
  | Set : ((Context.t option -> Set.t Slug.t -> 'w), 'w, Void.t) t
  | SetAdd : ('w, 'w, Void.t) t
  | Source : ((Context.t option -> Source.t Slug.t -> 'w), 'w, Void.t) t
  | SourceAdd : ('w, 'w, Void.t) t
  | Tune : ((Context.t option -> Tune.t Slug.t -> 'w), 'w, Void.t) t
  | TuneAdd : ('w, 'w, Void.t) t
  | Version : ((Context.t option -> Version.t Slug.t -> 'w), 'w, Void.t) t
  | VersionAdd : ((Tune.t Slug.t option -> 'w), 'w, Void.t) t
  | Index : ('w, 'w, Void.t) t
  | Explore : ((string option -> 'w), 'w, Void.t) t
  | AuthCreateUser : ('w, 'w, Void.t) t
  | AuthPasswordReset : ((string -> string -> 'w), 'w, Void.t) t
[@@deriving madge_wrapped_endpoints]

let all_endpoints' = all'

open Madge

(* FIXME: Factorise adding the model prefixes. *)
let route : type a w r. (a, w, r) t -> (a, w, r) route =
  let open Route in
  function
    | Book -> literal "book" @@ query_opt "context" (module Context) @@ variable (module SSlug(Book)) @@ void ()
    | BookAdd -> literal "book" @@ literal "add" @@ void ()
    | BookEdit -> literal "book" @@ literal "edit" @@ variable (module SSlug(Book)) @@ void ()
    | Dance -> literal "dance" @@ query_opt "context" (module Context) @@ variable (module SSlug(Dance)) @@ void ()
    | DanceAdd -> literal "dance" @@ literal "add" @@ void ()
    | Person -> literal "person" @@ query_opt "context" (module Context) @@ variable (module SSlug(Person)) @@ void ()
    | PersonAdd -> literal "person" @@ literal "add" @@ void ()
    | Set -> literal "set" @@ query_opt "context" (module Context) @@ variable (module SSlug(Set)) @@ void ()
    | SetAdd -> literal "set" @@ literal "add" @@ void ()
    | Source -> literal "source" @@ query_opt "context" (module Context) @@ variable (module SSlug(Source)) @@ void ()
    | SourceAdd -> literal "source" @@ literal "add" @@ void ()
    | Tune -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module SSlug(Tune)) @@ void ()
    | TuneAdd -> literal "tune" @@ literal "add" @@ void ()
    | Version -> literal "version" @@ query_opt "context" (module Context) @@ variable (module SSlug(Version)) @@ void ()
    | VersionAdd -> literal "version" @@ literal "add" @@ query_opt "tune" (module JSlug(Tune)) @@ void ()
    | Index -> void ()
    | Explore -> literal "explore" @@ query_opt "q" (module JString) @@ void ()
    | AuthCreateUser -> literal "auth" @@ literal "create-user" @@ void ()
    | AuthPasswordReset -> literal "auth" @@ literal "reset-password" @@ query "username" (module JString) @@ query "token" (module JString) @@ void ()

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
  let open Any in
  match any with
  | Version version -> href_version ?context (Entry.slug version)
  | Set set -> href_set ?context (Entry.slug set)
  | Person person -> href_person ?context (Entry.slug person)
  | Source source -> href_source ?context (Entry.slug source)
  | Dance dance -> href_dance ?context (Entry.slug dance)
  | Book book -> href_book ?context (Entry.slug book)
  | Tune tune -> href_tune ?context (Entry.slug tune)

let make_describe ~get_version ~get_tune ~get_set ~get_book ~get_dance ~get_person ~get_source = fun uri ->
  let describe : type a r. (a, (string * string) option Lwt.t, r) t -> a = function
    | Index -> Lwt.return_none
    | Explore -> Fun.const Lwt.return_none
    | VersionAdd -> Fun.const Lwt.return_none
    | TuneAdd -> Lwt.return_none
    | SetAdd -> Lwt.return_none
    | BookAdd -> Lwt.return_none
    | BookEdit -> Fun.const Lwt.return_none
    | PersonAdd -> Lwt.return_none
    | SourceAdd -> Lwt.return_none
    | DanceAdd -> Lwt.return_none
    | AuthCreateUser -> Lwt.return_none
    | AuthPasswordReset -> Fun.const2 Lwt.return_none
    | Version ->
      (fun _ slug ->
        let%lwt name = Lwt.bind (get_version slug) (Lwt.map Tune.name % (get_tune % Version.tune)) in
        Lwt.return @@ Some ("version", name)
      )
    | Tune ->
      (fun _ slug ->
        let%lwt name = Lwt.map Tune.name (get_tune slug) in
        Lwt.return @@ Some ("tune", name)
      )
    | Set ->
      (fun _ slug ->
        let%lwt name = Lwt.map Set.name (get_set slug) in
        Lwt.return @@ Some ("set", name)
      )
    | Book ->
      (fun _ slug ->
        let%lwt title = Lwt.map Book.title (get_book slug) in
        Lwt.return @@ Some ("book", title)
      )
    | Dance ->
      (fun _ slug ->
        let%lwt name = Lwt.map Dance.name (get_dance slug) in
        Lwt.return @@ Some ("dance", name)
      )
    | Person ->
      (fun _ slug ->
        let%lwt name = Lwt.map Person.name (get_person slug) in
        Lwt.return @@ Some ("person", name)
      )
    | Source ->
      (fun _ slug ->
        let%lwt name = Lwt.map Source.name (get_source slug) in
        Lwt.return @@ Some ("source", name)
      )
  in
  let madge_match_apply_all : (string * string) option Lwt.t wrapped' list -> (unit -> (string * string) option Lwt.t) option =
    List.map_first_some @@ fun (W' page) ->
    Madge.apply' (route page) (fun () -> describe page) {meth = GET; uri; body = ""}
  in
  match madge_match_apply_all @@ all_endpoints' () with
  | Some page -> page ()
  | None -> (* FIXME: 404 page *) assert false
