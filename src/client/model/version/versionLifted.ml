open Dancelor_common_model

include VersionLifter.Lift(Credit)(Tune)

let content _version = assert false (* FIXME *)

module E = VersionEndpoints
module A = E.Arguments

let get slug =
  Madge_client.(call ~endpoint: E.get
  @@ fun { a } _ ->
    a A.slug slug)
