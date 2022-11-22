(** {1 API Router}

    Router for Dancelor's API endpoints. This module links methods and paths to
    an abstract representation of API endpoints. Because this avoids linking to
    controllers in Dancelor's server, this module can be used in Dancelor's
    client, in particular to generate URIs. *)

open Nes
open Dancelor_common_model

type endpoint =
  | Index
  | MagicSearch (* FIXME: argument *)

  | CreditSave
  | Credit of CreditCore.t Slug.t

  | Dance of DanceCore.t Slug.t

  | PersonSave
  | Person of PersonCore.t Slug.t

  | BookAll
  | BookCompose
  | BookPdf of BookCore.t Slug.t
  | Book of BookCore.t Slug.t

  | SetAll
  | SetCompose
  | SetSave
  | SetLy of SetCore.t Slug.t
  | SetPdf of SetCore.t Slug.t
  | Set of SetCore.t Slug.t
  | SetDelete of SetCore.t Slug.t

  | Tune of TuneCore.t Slug.t

  | VersionAddition
  | VersionAll
  | VersionSearch
  | VersionLy of VersionCore.t Slug.t
  | VersionSvg of VersionCore.t Slug.t
  | VersionOgg of VersionCore.t Slug.t
  | VersionPdf of VersionCore.t Slug.t
  | Version of VersionCore.t Slug.t

  | Victor

val path_to_endpoint : meth:Cohttp.Code.meth -> path:string -> endpoint option

val path_of_endpoint : api_prefix:bool -> endpoint -> Cohttp.Code.meth * string
(** Given a controller, returns the method and the path to it. The path is
    prefixed by the API prefix, unless [api_prefix] is set to [false]. *)

val path_of_get_endpoint : api_prefix:bool -> endpoint -> string
(** Same as {!path_of_controller} but assumes a [GET] controller and fails
    otherwise. *)

val gpath : api:bool -> endpoint -> string
(** [gpath ~api con] is the same as [path_of_get_endpoint ~api_prefix:api con]. *)
