open Dancelor_common
open Model

include BookLifter.Lift(Dance)(Set)(Tune)(Version)

let get slug =
  Madge_client_new.call ApiRouter.(route @@ Book Get) (NesSlug.to_string slug)
