module Person = Generic.Make (Dancelor_common_model.Person)
module Credit = Generic.Make (Dancelor_common_model.Credit)
module Source = Generic.Make (Dancelor_common_model.Source)
module Dance = Generic.Make (Dancelor_common_model.Dance)
module Tune = Generic.Make (Dancelor_common_model.Tune)
module TuneGroup = Generic.Make (Dancelor_common_model.TuneGroup)
module Set = Generic.Make (Dancelor_common_model.Set)
module Program = Generic.Make (Dancelor_common_model.Program)

module Storage = Storage

type t =
  { person : Person.t ;
    credit : Credit.t ;
    source : Source.t ;
    dance : Dance.t ;
    tune : Tune.t ;
    tune_group : TuneGroup.t ;
    set : Set.t ;
    program : Program. t }

let initialise () =
  let%lwt person = Person.initialise () in
  let%lwt credit = Credit.initialise () in
  let%lwt source = Source.initialise () in
  let%lwt dance = Dance.initialise () in
  let%lwt tune = Tune.initialise () in
  let%lwt tune_group = TuneGroup.initialise () in
  let%lwt set = Set.initialise () in
  let%lwt program = Program.initialise () in
  Lwt.return { person; credit; source; dance; tune; tune_group; set; program }

let establish database =
  Person.Static.establish database.person;
  Credit.Static.establish database.credit;
  Source.Static.establish database.source;
  Dance.Static.establish database.dance;
  Tune.Static.establish database.tune;
  TuneGroup.Static.establish database.tune_group;
  Set.Static.establish database.set;
  Program.Static.establish database.program;
  Lwt.return ()
