open Dancelor_common
open Model

include VersionLifter.Lift(Person)(Tune)

let get = Madge_cohttp_lwt_client.call ApiRouter.(route @@ Version Get)
