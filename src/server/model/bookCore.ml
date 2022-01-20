open Nes
include Dancelor_common_model.BookCore

let contents book =
  let%lwt contents = contents book in
  Lwt_list.map_p
    (function
      | (Version (version, parameters) : page_slug) ->
        let%lwt version = Version.get version in
        Lwt.return (Version (version, parameters))
      | Set (set, parameters) ->
        let%lwt set = Set.get set in
        Lwt.return (Set (set, parameters))
      | InlineSet (set, parameters) ->
        Lwt.return (InlineSet (set, parameters)))
    contents

let warnings _book = assert false (* FIXME *)

module E = Dancelor_common_model.BookEndpoints
module A = E.Arguments

let get = Dancelor_server_database.Book.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )
