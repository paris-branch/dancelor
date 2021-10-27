module Kind = Kind
module Music = Music
module Pagination = Pagination
module Status = Status
module Score = Score
module Transposition = Transposition

module PersonCore = PersonCore
module type PERSON = module type of Person_signature
module Person_endpoints = Person_endpoints
module PersonFilter = PersonFilter

module CreditCore = CreditCore
module type CREDIT = module type of Credit_signature
module Credit_endpoints = Credit_endpoints
module CreditFilter = CreditFilter

module DanceCore = DanceCore
module type DANCE = module type of Dance_signature
module Dance_endpoints = Dance_endpoints
module DanceFilter = DanceFilter

module TuneCore = TuneCore
module type TUNE = module type of Tune_signature
module Tune_endpoints = Tune_endpoints
module TuneFilter = TuneFilter

module VersionCore = VersionCore
module type VERSION = module type of Version_signature
module Version_endpoints = Version_endpoints
module VersionFilter = VersionFilter
module VersionParameters = VersionParameters

module SetCore = SetCore
module type SET = module type of Set_signature
module Set_endpoints = Set_endpoints
module SetFilter = SetFilter
module SetParameters = SetParameters

module BookCore = BookCore
module type BOOK = module type of Book_signature
module Book_endpoints = Book_endpoints
module BookFilter = BookFilter
module BookParameters = BookParameters

module AnyCore = AnyCore
module type ANY = module type of Any_signature
module Any_endpoints = Any_endpoints
module AnyFilter = AnyFilter

module Formula = Formula
module TextFormula = TextFormula
