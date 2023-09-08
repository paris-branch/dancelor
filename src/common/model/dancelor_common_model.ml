(** {1 Model — Common Part}

    This module contains everything in the model that is common to both client
    and server implementation. Basically, each model (eg. set) is made of five
    modules:

    - {!SetCore} contains the core definitions of set, including its actual
      low-level implementation, getters and setters. This definition and these
      functions are at the core of all the other set modules and therefore they
      belongs to {!Dancelor_common_model}. Being low-level, they manipulate actual
      slugs of other models. The {!SetCore} module should never be used except in
      other core modules and in {!SetLifter}. It should certainly not appear in
      {!Dancelor_client} and {!Dancelor_server}.

    - {!SetSignature} is a signature file describing the high-level interface of
      the set model. In particular, all the getters and setters from {!SetCore} are
      present in a lifted form, that is in a form that actually manipulates other
      models and not just their slugs. In addition to those getters and setters,
      {!SetSignature} defines other helpers as well as API points belonging to the
      set model. The set model in {!Dancelor_client_model} and
      {!Dancelor_server_model} share this signature; therefore, it belongs to
      {!Dancelor_common_model}.

    - {!SetLifter} contains a unique functor, {!SetLifter.Lift} which lifts
      aforementioned low-level getters and setters, basically generating a module
      of type {!SetSignature}. Each functor depends on the lifted form of other
      models but, apart from that, the code is the same on both {!Dancelor_client}
      and {!Dancelor_server}. Therefore, it belongs to {!Dancelor_common_model}.

    - [SetLifted] contains the application of {!SetLifter.Lift}, giving a module
      providing most of the functions of {!SetSignature}. In addition to the result
      of {!SetLifter.Lift}, it contains the definition of {!SetSignature.get}, the
      function that recovers an actual set from a set slug. [SetLifted] exists in
      two different places, [Dancelor_client_model.SetLifted] and
      [Dancelor_server_model.SetLifted]. These two modules differ only in their
      implementation of {!SetSignature.get} (database access for the server, API
      call for the client) and in the arguments they give to {!SetLifter.Lift}.

    - [SetFilter] needs to be cleaned and integrated to the other modules. FIXME.

    - [Set], finally, gathers everything under the same hat. This is the only
      module that should be used outside of {!Dancelor_client_model} and
      {!Dancelor_server_model}. It is of type {!SetSignature}. It exists in two
      different places: {!Dancelor_client_model.Set} and
      {!Dancelor_server_model.Set}.

    This convoluted construction ensures that code in the client and the server
    look alike, as they both manipulate models of type {!SetSignature}. *)

(* FIXME: all the things that are not technically models should probably move to
   something else. Maybe Dancelor_common_utils? *)
module Date = Date
module Music = Music
module Pagination = Pagination
module Status = Status
module Score = Score
module Transposition = Transposition

module Kind = Kind

module CreditCore = CreditCore
module type CREDIT = module type of CreditSignature
module CreditLifter = CreditLifter
module CreditEndpoints = CreditEndpoints

module DanceCore = DanceCore
module type DANCE = module type of DanceSignature
module DanceLifter = DanceLifter
module DanceEndpoints = DanceEndpoints

module TuneCore = TuneCore
module type TUNE = module type of TuneSignature
module TuneLifter = TuneLifter
module TuneEndpoints = TuneEndpoints

module VersionCore = VersionCore
module type VERSION = module type of VersionSignature
module VersionLifter = VersionLifter
module VersionEndpoints = VersionEndpoints
module VersionParameters = VersionParameters

module SetOrder = SetOrder
module SetCore = SetCore
module type SET = module type of SetSignature
module SetLifter = SetLifter
module SetEndpoints = SetEndpoints
module SetParameters = SetParameters

module BookCore = BookCore
module type BOOK = module type of BookSignature
module BookLifter = BookLifter
module BookEndpoints = BookEndpoints
module BookParameters = BookParameters

module AnyCore = AnyCore
module type ANY = module type of AnySignature
module AnyLifter = AnyLifter
module AnyEndpoints = AnyEndpoints

module Formula = Formula
module TextFormula = TextFormula
