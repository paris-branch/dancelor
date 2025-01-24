(** {1 Client Router} *)

open Nes
open Dancelor_common_model

module Context = struct
  (** Context in which a page might exist. TODO: I wonder whether it'd be possible
      to simply use [page] here, having pages carry a “parent” [page option]. *)
  type t =
    | InSearch of string
    | InSet of Set.t Slug.t * int
    | InBook of Book.t Slug.t * int
  [@@deriving yojson, variants]

  let inSet' = inSet % Slug.unsafe_of_string
  let inBook' = inBook % Slug.unsafe_of_string
end
include Context

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [Any.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [* for.Core all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

type (_, _, _) page =
  | Book : ((Context.t option -> Book.t Slug.t -> 'w), 'w, Void.t) page
  | BookAdd : ('w, 'w, Void.t) page
  | BookEdit : ((Book.t Slug.t -> 'w), 'w, Void.t) page
  | Dance : ((Context.t option -> Dance.t Slug.t -> 'w), 'w, Void.t) page
  | DanceAdd : ('w, 'w, Void.t) page
  | Person : ((Context.t option -> Person.t Slug.t -> 'w), 'w, Void.t) page
  | PersonAdd : ('w, 'w, Void.t) page
  | Set : ((Context.t option -> Set.t Slug.t -> 'w), 'w, Void.t) page
  | SetAdd : ('w, 'w, Void.t) page
  | Tune : ((Context.t option -> Tune.t Slug.t -> 'w), 'w, Void.t) page
  | TuneAdd : ('w, 'w, Void.t) page
  | Version : ((Context.t option -> Version.t Slug.t -> 'w), 'w, Void.t) page
  | VersionAdd : ((Tune.t Slug.t option -> 'w), 'w, Void.t) page
  | Index : ('w, 'w, Void.t) page
  | Explore : ((string option -> 'w), 'w, Void.t) page

type 'w page_wrapped' =
  | W : ('a, 'w, 'r) page -> 'w page_wrapped'

(* FIXME: The order matters, which means that we should fix the routes; right
   now, they are just ambiguous. *)
let all_endpoints' = [
  W BookAdd;
  W BookEdit;
  W Book;
  W DanceAdd;
  W Dance;
  W PersonAdd;
  W Person;
  W SetAdd;
  W Set;
  W TuneAdd;
  W Tune;
  W VersionAdd;
  W Version;
  W Index;
  W Explore;
]

open Madge

(* FIXME: Factorise adding the model prefixes. *)
let route : type a w r. (a, w, r) page -> (a, w, r) route = function
  | Book -> literal "book" @@ query_opt "context" (module Context) @@ variable (module SSlug(Book)) @@ get (module Void)
  | BookAdd -> literal "book" @@ literal "add" @@ get (module Void)
  | BookEdit -> literal "book" @@ literal "edit" @@ variable (module SSlug(Book)) @@ get (module Void)
  | Dance -> literal "dance" @@ query_opt "context" (module Context) @@ variable (module SSlug(Dance)) @@ get (module Void)
  | DanceAdd -> literal "dance" @@ literal "add" @@ get (module Void)
  | Person -> literal "person" @@ query_opt "context" (module Context) @@ variable (module SSlug(Person)) @@ get (module Void)
  | PersonAdd -> literal "person" @@ literal "add" @@ get (module Void)
  | Set -> literal "set" @@ query_opt "context" (module Context) @@ variable (module SSlug(Set)) @@ get (module Void)
  | SetAdd -> literal "set" @@ literal "add" @@ get (module Void)
  | Tune -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module SSlug(Tune)) @@ get (module Void)
  | TuneAdd -> literal "tune" @@ literal "add" @@ get (module Void)
  | Version -> literal "version" @@ query_opt "context" (module Context) @@ variable (module SSlug(Version)) @@ get (module Void)
  | VersionAdd -> literal "version" @@ literal "add" @@ query_opt "tune" (module JSlug(Tune)) @@ get (module Void)
  | Index -> get (module Void)
  | Explore -> literal "explore" @@ query_opt "q" (module JString) @@ get (module Void)
(* FIXME: short for `get (module Void)` *)

let href : type a r. (a, string, r) page -> a = fun page ->
  process (route page) (fun (module _) {meth; uri; _} -> assert (meth = GET); Uri.to_string uri)

let href_book ?context book = href Book context book
let href_dance ?context dance = href Dance context dance
let href_person ?context person = href Person context person
let href_set ?context set = href Set context set
let href_tune ?context tune = href Tune context tune
let href_version ?context version = href Version context version
let href_versionAdd ?tune () = href VersionAdd tune

let href_any ?context any =
  let open Dancelor_common_database in
  let open Any in
  match any with
  | Version version -> href_version ?context (Entry.slug version)
  | Set set -> href_set ?context (Entry.slug set)
  | Person person -> href_person ?context (Entry.slug person)
  | Dance dance -> href_dance ?context (Entry.slug dance)
  | Book book -> href_book ?context (Entry.slug book)
  | Tune tune -> href_tune ?context (Entry.slug tune)

let make_describe ~get_version ~get_tune ~get_set ~get_book ~get_dance ~get_person = fun uri ->
  let describe : type a r. (a, (string * string) option Lwt.t, r) page -> a = function
    | Index -> Lwt.return None
    | Explore -> (fun _ -> Lwt.return None)
    | VersionAdd -> (fun _ -> Lwt.return None)
    | TuneAdd -> Lwt.return None
    | SetAdd -> Lwt.return None
    | BookAdd -> Lwt.return None
    | BookEdit -> (fun _ -> Lwt.return None)
    | PersonAdd -> Lwt.return None
    | DanceAdd -> Lwt.return None
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
  in
  let madge_match_apply_all : (string * string) option Lwt.t page_wrapped' list -> (unit -> (string * string) option Lwt.t) option =
    List.map_first_some @@ fun (W page) ->
    Madge.match_' (route page) (describe page) {meth = GET; uri; body = ""}
  in
  match madge_match_apply_all all_endpoints' with
  | Some page -> page ()
  | None -> (* FIXME: 404 page *) assert false
