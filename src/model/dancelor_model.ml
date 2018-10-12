module Credit = Credit
module Dance = Dance
module Kind = Kind
module Person = Person
module Set = Set
module Tune = Tune

module Database =
  struct
    let initialise () =
      Person.Database.initialise ();
      Credit.Database.initialise ();
      Tune.Database.initialise ()
  end
