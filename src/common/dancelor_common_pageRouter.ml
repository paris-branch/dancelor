(** {1 Client Router} *)

open Nes
open Dancelor_common_model

(** Context in which a page might exist. TODO: I wonder whether it'd be possible
    to simply use [page] here, having pages carry a “parent” [page option]. *)
type context =
  | InSearch of string
[@@deriving yojson]

let inSearch query = InSearch query

(** Existing pages in Dancelor's client. *)
type page =
  | Index
  | BookCompose
  | BookEdit of BookCore.t Slug.t
  | Book of {slug: BookCore.t Slug.t; context: context option}
  | PersonAdd
  | Person of {slug: PersonCore.t Slug.t; context: context option}
  | Dance of {slug: DanceCore.t Slug.t; context: context option}
  | Explore of string option
  | SetCompose
  | Set of {slug: SetCore.t Slug.t; context: context option}
  | Tune of {slug: TuneCore.t Slug.t; context: context option}
  | VersionAdd
  | Version of {slug : VersionCore.t Slug.t; context: context option}

(* FIXME: It would be so much nicer if [Search] could carry an actual
   [AnyCore.Filter.predicate Formula.t]. That however requires moving a lot of
   code from [*Lifter] to [*Core] for all models (basically everything but the
   [accepts] function, I would say), so, for now, we keep it as a string. *)

let book ?context slug = Book {slug; context}
let bookEdit slug = BookEdit slug
let person ?context slug = Person {slug; context}
let dance ?context slug = Dance {slug; context}
let explore q = Explore q
let set ?context slug = Set {slug; context}
let tune ?context slug = Tune {slug; context}
let version ?context slug = Version {slug; context}

let unBookEdit = function BookEdit slug -> Some slug | _ -> None

open Madge_router
module MQ = Madge_query

let context_of_query = MQ.get_ "context" context_of_yojson
let context_to_query = Option.fold ~none:MQ.empty ~some:(MQ.singleton "context" % context_to_yojson)

let routes =
  (* NOTE: It is important that [with_slug] instances come after more specific
     ones. For instance, the [with_slug] corresponding to "/book/{slug}" should
     come after "/book/all", so as to avoid matching "all" as a slug. *)
  [
    direct    `GET "/"                Index ;
    direct    `GET "/book/compose"    BookCompose ;
    with_slug `GET "/book/edit"      (bookEdit, unBookEdit) ;

    with_slug_and_query `GET "/book"
      (fun slug query -> book slug ?context:(context_of_query query))
      (function Book {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    direct    `GET "/person/add"      PersonAdd ;

    with_slug_and_query `GET "/person"
      (fun slug query -> person slug ?context:(context_of_query query))
      (function Person {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    with_slug_and_query `GET "/dance"
      (fun slug query -> dance slug ?context:(context_of_query query))
      (function Dance {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    with_query `GET "/explore"
      (fun query -> explore @@ MQ.get_string "q" query)
      (function
        | Explore None -> Some MQ.empty
        | Explore (Some q) -> Option.some @@ MQ.singleton "q" (`String q)
        | _ -> None) ;

    direct    `GET "/set/compose"     SetCompose ;

    with_slug_and_query `GET "/set"
      (fun slug query -> set slug ?context:(context_of_query query))
      (function Set {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    with_slug_and_query `GET "/tune"
      (fun slug query -> tune slug ?context:(context_of_query query))
      (function Tune {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;

    direct    `GET "/version/add"     VersionAdd ;

    with_slug_and_query `GET "/version"
      (fun slug query -> version slug ?context:(context_of_query query))
      (function Version {slug; context} -> Some (slug, context_to_query context) | _ -> None) ;
  ]

let path page =
  Madge_router.resource_to_request page routes
  |> Madge_router.request_to_uri
  |> Uri.to_string

(* Shorter versions for when we want immediately a path, which is the case the
   majority of the time. *)
let path_book ?context slug = path @@ book ?context slug
let path_bookEdit = path % bookEdit
let path_person ?context slug = path @@ person ?context slug
let path_dance ?context slug = path @@ dance ?context slug
let path_explore = path % explore
let path_set ?context slug = path @@ set ?context slug
let path_tune ?context slug = path @@ tune ?context slug
let path_version ?context slug = path @@ version ?context slug

let path_any ?context any =
  let open Dancelor_common_model in
  let open AnyCore in
  match any with
  | Version version -> path_version ?context (VersionCore.slug version)
  | Set set -> path_set ?context (SetCore.slug set)
  | Person person -> path_person ?context (PersonCore.slug person)
  | Dance dance -> path_dance ?context (DanceCore.slug dance)
  | Book book -> path_book ?context (BookCore.slug book)
  | Tune tune -> path_tune ?context (TuneCore.slug tune)
