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
  let%lwt person = Person.State.initialise () in
  let%lwt credit = Credit.State.initialise () in
  let%lwt source = Source.State.initialise () in
  let%lwt dance = Dance.State.initialise () in
  let%lwt tune = Tune.State.initialise () in
  let%lwt tune_group = TuneGroup.State.initialise () in
  let%lwt set = Set.State.initialise () in
  let%lwt program = Program.State.initialise () in
  Lwt.return { person; credit; source; dance; tune; tune_group; set; program }

let establish database =
  Person.establish database.person;
  Credit.establish database.credit;
  Source.establish database.source;
  Dance.establish database.dance;
  Tune.establish database.tune;
  TuneGroup.establish database.tune_group;
  Set.establish database.set;
  Program.establish database.program;
  Lwt.return ()
