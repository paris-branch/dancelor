open Nes
include Dancelor_common_model.BookCore

let contents p =
  let%lwt contents = contents p in
  Lwt_list.map_p
    (function
      | (Version (v, p) : page_slug) -> let%lwt v = Version.get v in Lwt.return (Version (v, p))
      | Set (s, p) -> let%lwt s = Set.get s in Lwt.return (Set (s, p))
      | InlineSet (s, p) -> Lwt.return (InlineSet (s, p)))
    contents

let warnings _p = assert false (* FIXME *)

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Book.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
