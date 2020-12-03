open Nes
include Dancelor_common_model.Book

let contents p =
  let%lwt contents = contents p in
  Lwt_list.map_p
    (function
      | (Version (v, p) : Self.page) -> let%lwt v = Version.get v in Lwt.return (Version (v, p))
      | Set (s, p) -> let%lwt s = Set.get s in Lwt.return (Set (s, p))
      | InlineSet (s, p) -> Lwt.return (InlineSet (s, p)))
    contents

let warnings _p = assert false (* FIXME *)

(* * *)

let get = Dancelor_server_database.Book.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let get_all () =
  let%lwt books = Dancelor_server_database.Book.get_all () in
  Lwt_list.(sort_multiple [
      increasing     date NesDate.compare ;
      increasing    title String.Sensible.compare ;
      increasing subtitle String.Sensible.compare
    ]) books

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get_all @@ fun _ _ ->
    get_all ()
  )

let search string book =
  let%lwt title = title book in
  String.inclusion_proximity ~char_equal:Char.Sensible.equal ~needle:string title
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Book.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing     date NesDate.compare ;
      increasing    title String.Sensible.compare ;
      increasing subtitle String.Sensible.compare
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )