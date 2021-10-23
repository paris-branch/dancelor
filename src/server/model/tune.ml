open Nes

module Self = struct
  include Dancelor_common_model.Tune

  let author = author >=>?| (Credit.get >=>| Lwt.return_some)
  let dances = dances >=>| Lwt_list.map_p Dance.get
end
include Self

module Filter = struct
  include Filter

  let accepts filter tune =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is tune' ->
      equal tune tune' >|=| Formula.interpret_bool

    | Name string ->
      let%lwt name = Self.name tune in
      Lwt.return (String.proximity ~char_equal string name)

    | NameMatches string ->
      let%lwt name = Self.name tune in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

    | Author afilter ->
      (match%lwt Self.author tune with
       | None -> Formula.interpret_false |> Lwt.return
       | Some author -> Credit.Filter.accepts afilter author)

    | Kind kind' ->
      let%lwt kind = Self.kind tune in
      Lwt.return (Formula.interpret_bool (kind = kind'))
end

module E = Dancelor_common_model.Tune_endpoints
module A = E.Arguments

let get = Dancelor_server_database.Tune.get

let () =
  Madge_server.(
    register ~endpoint:E.get @@ fun {a} _ ->
    get (a A.slug)
  )

let search ?pagination ?(threshold=0.) filter =
  Dancelor_server_database.Tune.get_all ()
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
    >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [
      increasing name String.Sensible.compare;
      increasing name String.compare_lengths;
    ])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold: (o A.threshold)
      (a A.filter)
  )
