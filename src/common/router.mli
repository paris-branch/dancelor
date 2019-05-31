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

  | Credit of Credit.t Slug.t

  | Dance of Dance.t Slug.t

  | Pascaline

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

  | TuneGroup of TuneGroup.t Slug.t

  | TuneAll
  | TuneSearch
  | TuneLy of Tune.t Slug.t
  | TunePng of Tune.t Slug.t
  | Tune of Tune.t Slug.t

  | Victor

val path_to_controller : meth:Cohttp.Code.meth -> path:string -> controller option

val path_of_controller : controller -> Cohttp.Code.meth * string
