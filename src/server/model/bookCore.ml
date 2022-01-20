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

let versions_from_contents book =
  let%lwt contents = contents book in
  Lwt_list.filter_map_p
    (function
      | Version (version, _) -> Lwt.return_some version
      | _ -> Lwt.return_none)
    contents

let sets_from_contents book =
  let%lwt contents = contents book in
  Lwt_list.filter_map_p
    (function
      | Version _ -> Lwt.return_none
      | Set (set, _) | InlineSet (set, _) -> Lwt.return_some set)
    contents

let unique_sets_from_contents book =
  let%lwt sets = sets_from_contents book in
  List.sort_uniq_lwt Set.compare sets

let sets_and_parameters_from_contents book =
  let%lwt contents = contents book in
  Lwt_list.filter_map_p
    (function
      | Set (set, parameters) | InlineSet (set, parameters) ->
        Lwt.return_some (set, parameters)
      | Version _ -> Lwt.return_none)
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
