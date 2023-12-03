(** {1 Server-side models} *)

module Common = Dancelor_common_model

(** {2 Modules overriden on the server side} *)

module Pagination = Pagination

module Person            : Common.PERSON  = Person
module Dance             : Common.DANCE   = Dance
module Tune              : Common.TUNE    = Tune

module Version           : Common.VERSION = Version
module VersionParameters                  = VersionParameters

module SetOrder                           = SetOrder
module Set               : Common.SET     = Set
module SetParameters                      = SetParameters

module Book              : Common.BOOK    = Book
module BookParameters                     = BookParameters

module Any               : Common.ANY     = Any

(** {2 Modules taken as-is from {!Dancelor_common}} *)

open Common

module Formula = Formula
module Kind = Kind
module Music = Music
module Score = Score
module Status = Status
module TextFormula = TextFormula
module Transposition = Transposition
