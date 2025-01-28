(** {1 Client-side models} *)

module Common = Dancelor_common

(* FIXME: out of model (into Utils) or out of Dancelor itself (into Nes) *)
module Formula = Common.Model.Formula
module IssueReport = Common.Model.IssueReport
module Kind = Common.Model.Kind
module Music = Common.Model.Music
module Search = Common.Model.Search
module TextFormula = Common.Model.TextFormula

(* reproduced in the client models *)
module Slice = Slice

(** {2 Modules overriden on the client side} *)

module Person : Common.Model.Person.S = Person
module Dance : Common.Model.Dance.S = Dance
module Tune : Common.Model.Tune.S = Tune
module Version : Common.Model.Version.S = Version
module VersionParameters = VersionParameters
module SetOrder = SetOrder
module Set : Common.Model.Set.S = Set
module SetParameters = SetParameters
module Book : Common.Model.Book.S = Book
module BookParameters = BookParameters
module Any : Common.Model.Any.S = Any
