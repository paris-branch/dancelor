module Credit = Credit
module Dance = Dance
module Kind = Kind
module Person = Person
module Set = Set
module Tune = Tune
module TuneGroup = TuneGroup
module Program = Program
module Music = Music

module Database =
  struct
    let initialise () =
      Person.Database.initialise ();
      Credit.Database.initialise ();
      TuneGroup.Database.initialise ();
      Tune.Database.initialise ();
      Set.Database.initialise ();
      Program.Database.initialise ()

    let report_without_accesses () =
      Person.Database.report_without_accesses ();
      Credit.Database.report_without_accesses ();
      TuneGroup.Database.report_without_accesses ()
      (* only those who cannot live alone. *)
  end
