module Kind = Kind
module Music = Music
module Pagination = Pagination
module Status = Status
module Score = Score
module Transposition = Transposition

module Person = Person
module type PERSON = module type of Person_signature
module Person_endpoints = Person_endpoints

module Credit = Credit
module type CREDIT = module type of Credit_signature
module Credit_endpoints = Credit_endpoints

module Source = Source
module type SOURCE = module type of Source_signature
module Source_endpoints = Source_endpoints

module Dance = Dance
module type DANCE = module type of Dance_signature
module Dance_endpoints = Dance_endpoints

module Tune = Tune
module type TUNE = module type of Tune_signature
module Tune_endpoints = Tune_endpoints

module Version = Version
module type VERSION = module type of Version_signature
module Version_endpoints = Version_endpoints

module VersionFilter = VersionFilter
module VersionParameters = VersionParameters

module Set = Set
module type SET = module type of Set_signature
module Set_endpoints = Set_endpoints

module SetParameters = SetParameters

module Book = Book
module type BOOK = module type of Book_signature
module Book_endpoints = Book_endpoints

module BookParameters = BookParameters

module Any = Any
module type ANY = module type of Any_signature
module Any_endpoints = Any_endpoints
