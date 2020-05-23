module Person = Generic.Make (
  struct
    include Dancelor_common_model.Person

    let dependencies _ = Lwt.return []
  end)

module Credit = Generic.Make (
  struct
    include Dancelor_common_model.Credit

    let dependencies _ = Lwt.return []
  end)

module Source = Generic.Make (
  struct
    include Dancelor_common_model.Source

    let dependencies _ = Lwt.return []
  end)

module Dance = Generic.Make (
  struct
    include Dancelor_common_model.Dance

    let dependencies _ = Lwt.return []
  end)

module Tune = Generic.Make (
  struct
    include Dancelor_common_model.Tune

    let dependencies _ = Lwt.return []
  end)

module TuneGroup = Generic.Make (
  struct
    include Dancelor_common_model.TuneGroup

    let dependencies _ = Lwt.return []
  end)

module Set = Generic.Make (
  struct
    include Dancelor_common_model.Set

    let dependencies _ = Lwt.return []
  end)

module Program = Generic.Make (
  struct
    include Dancelor_common_model.Program

    let dependencies _ = Lwt.return []
  end)

module Storage = Storage

module State = struct
  type t =
    { person : Person.State.t ;
      credit : Credit.State.t ;
      source : Source.State.t ;
      dance : Dance.State.t ;
      tune : Tune.State.t ;
      tune_group : TuneGroup.State.t ;
      set : Set.State.t ;
      program : Program.State.t }

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
end
