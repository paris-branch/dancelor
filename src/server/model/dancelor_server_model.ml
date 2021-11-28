module Common = Dancelor_common_model

module Music  = Music
module Pagination = Pagination
module Status = Status
module Score  = Score
module Transposition = Transposition
module Formula = Formula
module TextFormula = TextFormula

module Kind                               = Kind
module KindFilter                         = KindFilter

module Person            : Common.PERSON  = Person
module PersonFilter                       = PersonFilter

module Credit            : Common.CREDIT  = Credit
module CreditFilter                       = CreditFilter

module Dance             : Common.DANCE   = Dance
module DanceFilter                        = DanceFilter

module Tune              : Common.TUNE    = Tune
module TuneFilter                         = TuneFilter

module Version           : Common.VERSION = Version
module VersionFilter                      = VersionFilter
module VersionParameters                  = VersionParameters

module SetOrder                           = SetOrder
module Set               : Common.SET     = Set
module SetFilter                          = SetFilter
module SetParameters                      = SetParameters

module Book              : Common.BOOK    = Book
module BookFilter                         = BookFilter
module BookParameters                     = BookParameters

module Any               : Common.ANY     = Any
module AnyFilter                          = AnyFilter
