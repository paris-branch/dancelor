open Dancelor_common
open Model

include VersionLifter.Lift(Person)(Tune)

let content _version = assert false (* FIXME *)

let get = Madge_client_new.call ApiRouter.(route @@ Version Get)
