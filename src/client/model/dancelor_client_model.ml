module Common = Dancelor_common_model

module Kind   = Kind
module Music  = Music
module Pagination = Pagination
module Status = Status
module Score  = Score
module Transposition = Transposition

module Person        : Common.Person.S        = Person
module Credit        : Common.Credit.S        = Credit
module Source        : Common.Source.S        = Source
module Dance         : Common.Dance.S         = Dance
module Tune          : Common.Tune.S          = Tune
module Version       : Common.Version.S       = Version
module VersionFilter : Common.VersionFilter.S = VersionFilter
module VersionParameters                      = VersionParameters
module Set           : Common.Set.S           = Set
module SetParameters                          = SetParameters
module Book       : Common.Book.S       = Book
module BookParameters                      = BookParameters

module Any           : Common.Any.S           = Any
