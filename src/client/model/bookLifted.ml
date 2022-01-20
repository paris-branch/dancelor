open Dancelor_common_model

include BookLifted.Make(Version)(Set)(Dance)

let get the_slug =
  let open BookEndpoints in
  let open Arguments in
  Madge_client.(
    call ~endpoint:get @@ fun {a} _ ->
    a slug the_slug
  )
