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

let path_new : type a r. (a, string, r) route -> a = fun route ->
  process route (fun (module _) uri -> Uri.to_string uri)

let path_book ?context book = path_new (route Book) context book
let path_dance ?context dance = path_new (route Dance) context dance
let path_person ?context person = path_new (route Person) context person
let path_set ?context set = path_new (route Set) context set
let path_tune ?context tune = path_new (route Tune) context tune
let path_version ?context version = path_new (route Version) context version
let path_versionAdd ?tune () = path_new (route VersionAdd) tune

let path_any ?context any =
  let open Dancelor_common_model in
  let open AnyCore in
  match any with
  | Version version -> path_version ?context (VersionCore.slug version)
  | Set set -> path_set ?context (SetCore.slug set)
  | Person person -> path_person ?context (PersonCore.slug person)
  | Dance dance -> path_dance ?context (DanceCore.slug dance)
  | Book book -> path_book ?context @@ BookCore.slug book
  | Tune tune -> path_tune ?context (TuneCore.slug tune)
