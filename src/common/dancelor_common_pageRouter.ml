(** {1 Client Router} *)

open Nes
open Dancelor_common_model

module Context = struct
  (** Context in which a page might exist. TODO: I wonder whether it'd be possible
      to simply use [page] here, having pages carry a “parent” [page option]. *)
  type t =
    | InSearch of string
    | InSet of SetCore.t Slug.t * int
    | InBook of BookCore.t Slug.t * int
  [@@deriving yojson, variants]

  let inSet' = inSet % Slug.unsafe_of_string
  let inBook' = inBook % Slug.unsafe_of_string
end
include Context

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [AnyCore.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [*Core] for all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

type (_, _, _) page =
  | Book : ((Context.t option -> BookCore.t Slug.t -> 'w), 'w, Void.t) page
  | BookAdd : ('w, 'w, Void.t) page
  | BookEdit : ((BookCore.t Slug.t -> 'w), 'w, Void.t) page
  | Dance : ((Context.t option -> DanceCore.t Slug.t -> 'w), 'w, Void.t) page
  | DanceAdd : ('w, 'w, Void.t) page
  | Person : ((Context.t option -> PersonCore.t Slug.t -> 'w), 'w, Void.t) page
  | PersonAdd : ('w, 'w, Void.t) page
  | Set : ((Context.t option -> SetCore.t Slug.t -> 'w), 'w, Void.t) page
  | SetAdd : ('w, 'w, Void.t) page
  | Tune : ((Context.t option -> TuneCore.t Slug.t -> 'w), 'w, Void.t) page
  | TuneAdd : ('w, 'w, Void.t) page
  | Version : ((Context.t option -> VersionCore.t Slug.t -> 'w), 'w, Void.t) page
  | VersionAdd : ((TuneCore.t Slug.t option -> 'w), 'w, Void.t) page
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
  | Book -> literal "book" @@ query_opt "context" (module Context) @@ variable (module SSlug(BookCore)) @@ return (module Void)
  | BookAdd -> literal "book" @@ literal "add" @@ return (module Void)
  | BookEdit -> literal "book" @@ literal "edit" @@ variable (module SSlug(BookCore)) @@ return (module Void)
  | Dance -> literal "dance" @@ query_opt "context" (module Context) @@ variable (module SSlug(DanceCore)) @@ return (module Void)
  | DanceAdd -> literal "dance" @@ literal "add" @@ return (module Void)
  | Person -> literal "person" @@ query_opt "context" (module Context) @@ variable (module SSlug(PersonCore)) @@ return (module Void)
  | PersonAdd -> literal "person" @@ literal "add" @@ return (module Void)
  | Set -> literal "set" @@ query_opt "context" (module Context) @@ variable (module SSlug(SetCore)) @@ return (module Void)
  | SetAdd -> literal "set" @@ literal "add" @@ return (module Void)
  | Tune -> literal "tune" @@ query_opt "context" (module Context) @@ variable (module SSlug(TuneCore)) @@ return (module Void)
  | TuneAdd -> literal "tune" @@ literal "add" @@ return (module Void)
  | Version -> literal "version" @@ query_opt "context" (module Context) @@ variable (module SSlug(VersionCore)) @@ return (module Void)
  | VersionAdd -> literal "version" @@ literal "add" @@ query_opt "tune" (module JSlug(TuneCore)) @@ return (module Void)
  | Index -> return (module Void)
  | Explore -> literal "explore" @@ query_opt "q" (module JString) @@ return (module Void)
(* FIXME: short for `return (module Void)` *)

let href : type a r. (a, string, r) page -> a = fun page ->
  process (route page) (fun (module _) uri -> Uri.to_string uri)

let href_book ?context book = href Book context book
let href_dance ?context dance = href Dance context dance
let href_person ?context person = href Person context person
let href_set ?context set = href Set context set
let href_tune ?context tune = href Tune context tune
let href_version ?context version = href Version context version
let href_versionAdd ?tune () = href VersionAdd tune

let href_any ?context any =
  let open Dancelor_common_model in
  let open AnyCore in
  match any with
  | Version version -> href_version ?context (VersionCore.slug version)
  | Set set -> href_set ?context (SetCore.slug set)
  | Person person -> href_person ?context (PersonCore.slug person)
  | Dance dance -> href_dance ?context (DanceCore.slug dance)
  | Book book -> href_book ?context @@ BookCore.slug book
  | Tune tune -> href_tune ?context (TuneCore.slug tune)

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
         let%lwt name = Lwt.bind (get_version slug) (Lwt.map TuneCore.name % (get_tune % VersionCore.tune)) in
         Lwt.return @@ Some ("version", name)
      )
    | Tune ->
      (fun _ slug ->
         let%lwt name = Lwt.map TuneCore.name (get_tune slug) in
         Lwt.return @@ Some ("tune", name)
      )
    | Set ->
      (fun _ slug ->
         let%lwt name = Lwt.map SetCore.name (get_set slug) in
         Lwt.return @@ Some ("set", name)
      )
    | Book ->
      (fun _ slug ->
         let%lwt title = Lwt.map BookCore.title (get_book slug) in
         Lwt.return @@ Some ("book", title)
      )
    | Dance ->
      (fun _ slug ->
         let%lwt name = Lwt.map DanceCore.name (get_dance slug) in
         Lwt.return @@ Some ("dance", name)
      )
    | Person ->
      (fun _ slug ->
         let%lwt name = Lwt.map PersonCore.name (get_person slug) in
         Lwt.return @@ Some ("person", name)
      )
  in
  let madge_match_apply_all : (string * string) option Lwt.t page_wrapped' list -> (unit -> (string * string) option Lwt.t) option =
    List.map_first_some @@ fun (W page) ->
    Madge.match_' (route page) (describe page) uri
  in
  match madge_match_apply_all all_endpoints' with
  | Some page -> page ()
  | None -> (* FIXME: 404 page *) assert false
