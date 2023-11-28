(** {1 Client Router} *)

open Nes
open Dancelor_common_model

(** Existing pages in Dancelor's client. *)
type page =
  | Index
  | BookAll
  | BookCompose
  | BookEdit of BookCore.t Slug.t
  | Book of BookCore.t Slug.t
  | PersonAdd
  | Person of PersonCore.t Slug.t
  | Dance of DanceCore.t Slug.t
  | Search of string option
  | SetAll
  | SetCompose
  | Set of SetCore.t Slug.t
  | Tune of TuneCore.t Slug.t
  | VersionAdd
  | VersionAll
  | VersionBroken
  | Version of VersionCore.t Slug.t

let book slug = Book slug
let bookEdit slug = BookEdit slug
let person slug = Person slug
let dance slug = Dance slug
let search q = Search q
let set slug = Set slug
let tune slug = Tune slug
let version slug = Version slug

let unBook = function Book slug -> Some slug | _ -> None
let unBookEdit = function BookEdit slug -> Some slug | _ -> None
let unPerson = function Person slug -> Some slug | _ -> None
let unDance = function Dance slug -> Some slug | _ -> None
let unSet = function Set slug -> Some slug | _ -> None
let unTune = function Tune slug -> Some slug | _ -> None
let unVersion = function Version slug -> Some slug | _ -> None

open Madge_router
module MQ = Madge_query

let routes =
  (* NOTE: It is important that [with_slug] instances come after more specific
     ones. For instance, the [with_slug] corresponding to "/book/{slug}" should
     come after "/book/all", so as to avoid matching "all" as a slug. *)
  [
    direct    `GET "/"                Index ;
    direct    `GET "/book/all"        BookAll ;
    direct    `GET "/book/compose"    BookCompose ;
    with_slug `GET "/book/edit"      (bookEdit, unBookEdit) ;
    with_slug `GET "/book"           (book, unBook) ;
    direct    `GET "/person/add"      PersonAdd ;
    with_slug `GET "/person"         (person, unPerson) ;
    with_slug `GET "/dance"          (dance, unDance) ;

    with_query `GET "/search"
      (fun query -> search @@ MQ.get_string "q" query)
      (function
        | Search None -> Some MQ.empty
        | Search (Some q) -> Option.some @@ MQ.singleton "q" (`String q)
        | _ -> None) ;

    direct    `GET "/set/all"         SetAll ;
    direct    `GET "/set/compose"     SetCompose ;
    with_slug `GET "/set"            (set, unSet) ;
    with_slug `GET "/tune"           (tune, unTune) ;
    direct    `GET "/version/add"     VersionAdd ;
    direct    `GET "/version/all"     VersionAll ;
    direct    `GET "/version/broken"  VersionBroken ;
    with_slug `GET "/version"        (version, unVersion) ;
  ]

let path page =
  Madge_router.resource_to_request page routes
  |> Madge_router.request_to_uri
  |> Uri.to_string

(* Shorter versions for when we want immediately a path, which is the case the
   majority of the time. *)
let path_book = path % book
let path_bookEdit = path % bookEdit
let path_person = path % person
let path_dance = path % dance
let path_search = path % search
let path_set = path % set
let path_tune = path % tune
let path_version = path % version
