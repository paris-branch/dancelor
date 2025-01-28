(** {1 Model — Common Part}

    This module contains everything in the model that is common to both client
    and server implementation. Basically, each model (eg. set) is made of five
    modules:

    - {!SetCore} contains the core definitions of set, including its actual
      low-level implementation, getters and setters. This definition and these
      functions are at the core of all the other set modules and therefore they
      belongs to {!Dancelor_common.Model}. Being low-level, they manipulate actual
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
      {!Dancelor_common.Model}.

    - {!SetLifter} contains a unique functor, {!SetLifter.Lift} which lifts
      aforementioned low-level getters and setters, basically generating a module
      of type {!SetSignature}. Each functor depends on the lifted form of other
      models but, apart from that, the code is the same on both {!Dancelor_client}
      and {!Dancelor_server}. Therefore, it belongs to {!Dancelor_common.Model}.

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

(* FIXME: out of model *)
module Endpoints = Endpoints

(* FIXME: into Set *)
module SetOrder = SetOrder

(** Helper to make search controllers. *)
module Search = Search

module Any = struct
  include Core.Any
  module Filter = Filter.Any
  include Lifter.Any
  include Signature.Any
end

module Book = struct
  include Core.Book
  module Filter = Filter.Book
  include Lifter.Book
  include Signature.Book
end
module BookParameters = Core.BookParameters

module Dance = struct
  include Core.Dance
  module Filter = Filter.Dance
  include Lifter.Dance
  include Signature.Dance
end

module Person = struct
  include Core.Person
  module Filter = Filter.Person
  module Lift () = struct
    include Core.Person
    module Filter = Filter
  end
  include Signature.Person
end

module Set = struct
  include Core.Set
  module Filter = Filter.Set
  include Lifter.Set
  include Signature.Set
end
module SetParameters = Core.SetParameters

module Tune = struct
  include Core.Tune
  module Filter = Filter.Tune
  include Lifter.Tune
  include Signature.Tune
end

module Version = struct
  include Core.Version
  module Parameters = Core.VersionParameters
  module Filter = Filter.Version
  include Lifter.Version
  include Signature.Version
end
module VersionParameters = Core.VersionParameters
