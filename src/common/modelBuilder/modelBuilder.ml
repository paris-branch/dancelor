(** {1 Model builders}

    This module contains everything in the model that is common to both client
    and server implementation. Basically, each model (eg. set) is made of five
    modules:

    - {!SetCore} contains the core definitions of set, including its actual
      low-level implementation, getters and setters. This definition and these
      functions are at the core of all the other set modules and therefore they
      belongs to {!Dancelor_common.Model}. Being low-level, they manipulate actual
      slugs of other models. The {!SetCore} module should never be used except in
      other core modules and in {!SetBuilder}. It should certainly not appear in
      {!Dancelor_client} and {!Dancelor_server}.

    - {!SetSignature} is a signature file describing the high-level interface of
      the set model. In particular, all the getters and setters from {!SetCore} are
      present in a lifted form, that is in a form that actually manipulates other
      models and not just their slugs. In addition to those getters and setters,
      {!SetSignature} defines other helpers as well as API points belonging to the
      set model. The set model in {!Dancelor_client_model} and
      {!Dancelor_server_model} share this signature; therefore, it belongs to
      {!Dancelor_common.Model}.

    - {!SetBuilder} contains a unique functor, {!SetBuilder.Lift} which lifts
      aforementioned low-level getters and setters, basically generating a module
      of type {!SetSignature}. Each functor depends on the lifted form of other
      models but, apart from that, the code is the same on both {!Dancelor_client}
      and {!Dancelor_server}. Therefore, it belongs to {!Dancelor_common.Model}.

    - [SetLifted] contains the application of {!SetBuilder.Lift}, giving a module
      providing most of the functions of {!SetSignature}. In addition to the result
      of {!SetBuilder.Lift}, it contains the definition of {!SetSignature.get}, the
      function that recovers an actual set from a set slug. [SetLifted] exists in
      two different places, [Dancelor_client_model.SetLifted] and
      [Dancelor_server_model.SetLifted]. These two modules differ only in their
      implementation of {!SetSignature.get} (database access for the server, API
      call for the client) and in the arguments they give to {!SetBuilder.Lift}.

    - [SetFilter] needs to be cleaned and integrated to the other modules. FIXME.

    - [Set], finally, gathers everything under the same hat. This is the only
      module that should be used outside of {!Dancelor_client_model} and
      {!Dancelor_server_model}. It is of type {!SetSignature}. It exists in two
      different places: {!Dancelor_client_model.Set} and
      {!Dancelor_server_model.Set}.

    This convoluted construction ensures that code in the client and the server
    look alike, as they both manipulate models of type {!SetSignature}. *)

open Nes

module Core = Core
module Filter = Filter
module Builder = Builder
module Signature = Signature

module type Getters = sig
  val get_book : Core.Book.t Slug.t -> Core.Book.t Entry.t Lwt.t
  val get_dance : Core.Dance.t Slug.t -> Core.Dance.t Entry.t Lwt.t
  val get_person : Core.Person.t Slug.t -> Core.Person.t Entry.t Lwt.t
  val get_set : Core.Set.t Slug.t -> Core.Set.t Entry.t Lwt.t
  val get_source : Core.Source.t Slug.t -> Core.Source.t Entry.t Lwt.t
  val get_tune : Core.Tune.t Slug.t -> Core.Tune.t Entry.t Lwt.t
  val get_version : Core.Version.t Slug.t -> Core.Version.t Entry.t Lwt.t
end

module Build (G : Getters) = struct
  module Person : Signature.Person.S = struct
    include Core.Person
    module Filter = Filter.Person
    let get = G.get_person
  end

  module Source : Signature.Source.S = struct
    include Core.Source
    module Filter = Filter.Source
    let get = G.get_source
  end

  module User : Signature.User.S = struct
    include Builder.User.Build(Person)
  end

  module Dance : Signature.Dance.S = struct
    include Builder.Dance.Build(Person)
    let get = G.get_dance
  end

  module Tune : Signature.Tune.S = struct
    include Builder.Tune.Build(Dance)(Person)
    let get = G.get_tune
  end

  module Version : Signature.Version.S = struct
    include Builder.Version.Build(Source)(Person)(Tune)
    let get = G.get_version
  end
  module VersionParameters = struct
    include Core.VersionParameters
    let for_dance p =
      let%olwt dance_slug = Lwt.return (for_dance p) in
      let%lwt dance = G.get_dance dance_slug in
      Lwt.return_some dance
  end

  module Set : Signature.Set.S = struct
    include Builder.Set.Build(Dance)(Person)(Tune)(Version)
    let get = G.get_set
  end
  module SetOrder = SetOrder
  module SetParameters = struct
    include Core.SetParameters
    let make' ?forced_pages ?show_deviser ?show_order ?display_name ?for_dance ?every_version () =
      let for_dance = Option.map Entry.slug for_dance in
      Lwt.return @@ make ?forced_pages ?show_deviser ?show_order ?display_name ?for_dance ?every_version ()
    let for_dance p =
      let%olwt dance_slug = Lwt.return (for_dance p) in
      let%lwt dance = G.get_dance dance_slug in
      Lwt.return_some dance
  end

  module Book : Signature.Book.S = struct
    include Builder.Book.Build(Dance)(Person)(Set)(Tune)(Version)
    let get = G.get_book
  end
  module BookParameters = Core.BookParameters

  module Any : Signature.Any.S = Builder.Any.Build(Book)(Dance)(Person)(Set)(Source)(Tune)(Version)
end
