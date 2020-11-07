open Nes
include Dancelor_common_model.Program

let sets_and_parameters program =
  let%lwt sets_and_parameters = sets_and_parameters program in
  Lwt_list.map_p
    (fun (slug, parameters) ->
       let%lwt set = Set.get slug in
       Lwt.return (set, parameters))
    sets_and_parameters

let warnings _p = assert false (* FIXME *)

(* * *)

let get = Dancelor_server_database.Program.get

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get @@ fun {a} _ ->
    get (a Arg.slug)
  )

let get_all = Dancelor_server_database.Program.get_all

let () =
  Madge_server.(
    register ~endpoint:Endpoint.get_all @@ fun _ _ ->
    get_all ()
  )

let search string person =
  let%lwt name = name person in
  String.sensible_inclusion_proximity ~needle:string name
  |> Lwt.return

let search ?pagination ?(threshold=0.) string =
  Dancelor_server_database.Program.get_all ()
  >>=| Score.lwt_map_from_list (search string)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.list_proj_sort_decreasing ~proj:name String.sensible_compare
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:Endpoint.search @@ fun {a} {o} ->
    search
      ?pagination:(o Arg.pagination)
      ?threshold: (o Arg.threshold)
      (a Arg.string)
  )
