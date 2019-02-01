open Dancelor_model

(** This module is the link between requests (method + path) and
   controllers. Instead of linking directly to functions in
   [Dancelor_controller], it works on an abstract representation of
   controllers. This allows the client (in [js_of_ocaml]) to not
   depend on [Unix] and still use this module. Also, it allows to ask
   for the generation of links. *)

type controller =
  | Index
  | Credit of Credit.t
  | Pascaline
  | Person of Person.t
  | ProgramAll
  | ProgramPdf of Program.t
  | Program of Program.t
  | SetAll
  | SetCompose
  | SetSave
  | SetLy of Set.t
  | SetPdf of Set.t
  | Set of Set.t
  | TuneGroup of TuneGroup.t
  | TuneAll
  | TuneLy of Tune.t
  | TunePng of Tune.t
  | Tune of Tune.t
  | Victor

val path_to_controller : meth:Cohttp.Code.meth -> path:string -> controller option

val path_of_controller : controller -> (Cohttp.Code.meth * string) option
