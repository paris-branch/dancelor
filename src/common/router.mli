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

  | CreditSave
  | Credit of Credit.t Slug.t

  | Dance of Dance.t Slug.t

  | Pascaline

  | PersonSave
  | Person of Person.t Slug.t

  | ProgramAll
  | ProgramPdf of Program.t Slug.t
  | Program of Program.t Slug.t

  | SetAll
  | SetCompose
  | SetSave
  | SetLy of Set.t Slug.t
  | SetPdf of Set.t Slug.t
  | Set of Set.t Slug.t
  | SetDelete of Set.t Slug.t

  | Tune of Tune.t Slug.t

  | VersionAll
  | VersionSearch
  | VersionLy of Version.t Slug.t
  | VersionSvg of Version.t Slug.t
  | VersionPdf of Version.t Slug.t
  | Version of Version.t Slug.t

  | Victor

val path_to_controller : meth:Cohttp.Code.meth -> path:string -> controller option

val path_of_controller : controller -> Cohttp.Code.meth * string
