module Music = Music
module Pagination = Pagination
module Status = Status
module Score = Score
module Transposition = Transposition

module Kind = Kind
module KindFilter = KindFilter

module PersonCore = PersonCore
module type PERSON = module type of PersonSignature
module PersonEndpoints = PersonEndpoints
module PersonFilter = PersonFilter

module CreditCore = CreditCore
module type CREDIT = module type of CreditSignature
module CreditEndpoints = CreditEndpoints
module CreditFilter = CreditFilter

module DanceCore = DanceCore
module type DANCE = module type of DanceSignature
module DanceEndpoints = DanceEndpoints
module DanceFilter = DanceFilter

module TuneCore = TuneCore
module type TUNE = module type of TuneSignature
module TuneEndpoints = TuneEndpoints
module TuneFilter = TuneFilter

module VersionCore = VersionCore
module type VERSION = module type of VersionSignature
module VersionEndpoints = VersionEndpoints
module VersionFilter = VersionFilter
module VersionParameters = VersionParameters

module SetOrder = SetOrder
module SetCore = SetCore
module type SET = module type of SetSignature
module SetEndpoints = SetEndpoints
module SetFilter = SetFilter
module SetParameters = SetParameters

module BookCore = BookCore
module type BOOK = module type of BookSignature
module BookEndpoints = BookEndpoints
module BookFilter = BookFilter
module BookParameters = BookParameters

module AnyCore = AnyCore
module type ANY = module type of AnySignature
module AnyEndpoints = AnyEndpoints
module AnyFilter = AnyFilter

module Formula = Formula
module TextFormula = TextFormula
