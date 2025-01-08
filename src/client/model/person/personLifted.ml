open Dancelor_common
open Model

include PersonLifter.Lift ()

let get slug =
  Madge_client_new.call ApiRouter.(route @@ Person Get) (NesSlug.to_string slug)
