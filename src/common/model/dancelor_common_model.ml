module Kind   = Kind
module Music  = Music
module Status = Status
module Score = Score

module type PERSON    = module type of PersonExtended
module type CREDIT    = module type of CreditExtended
module type TUNEGROUP = module type of TuneGroupExtended
module type TUNE      = module type of TuneExtended
module type SET       = module type of SetExtended
module type PROGRAM   = module type of ProgramExtended

module Person    = Person
module Credit    = Credit
module TuneGroup = TuneGroup
module Tune      = Tune
module Set       = Set
module Program   = Program
