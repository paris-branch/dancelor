open Nes
open Dancelor_common_model

(** This module is the link between requests (method + path) and
    controllers. Instead of linking directly to functions in
    [Dancelor_controller], it works on an abstract representation of
    controllers. This allows the client (in [js_of_ocaml]) to not
    depend on [Unix] and still use this module. Also, it allows to ask
    for the generation of links. *)

type controller =
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

val path_to_controller : meth:Cohttp.Code.meth -> path:string -> controller option

val path_of_controller : ?api_prefix:bool -> controller -> Cohttp.Code.meth * string
(** Given a controller, returns the method and the path to it. The path is
    prefixed by the API prefix, unless [api_prefix] is set to [false]. *)

val path_of_get_controller : ?api_prefix:bool -> controller -> string
(** Same as {!path_of_controller} but assumes a [GET] controller and fails
    otherwise. *)

val gpath : ?api:bool -> controller -> string
(** [gpath ?api con] is the same as [path_of_get_controller ?api_prefix:?api con]. *)
