include Dancelor_common_model.Search

module Results = struct
  include CommonResults

  let persons r =
    let%lwt (n, persons) = persons r in
    let%lwt persons = Score.list_map_lwt_p Person.get persons in
    Lwt.return (n, persons)

  let credits r =
    let%lwt (n, credits) = credits r in
    let%lwt credits = Score.list_map_lwt_p Credit.get credits in
    Lwt.return (n, credits)

  let versions r =
    let%lwt (n, versions) = versions r in
    let%lwt versions = Score.list_map_lwt_p Version.get versions in
    Lwt.return (n, versions)

  let tunes r =
    let%lwt (n, tunes) = tunes r in
    let%lwt tunes = Score.list_map_lwt_p Tune.get tunes in
    Lwt.return (n, tunes)

  let programs r =
    let%lwt (n, programs) = programs r in
    let%lwt programs = Score.list_map_lwt_p Program.get programs in
    Lwt.return (n, programs)

  let sets r =
    let%lwt (n, sets) = sets r in
    let%lwt sets = Score.list_map_lwt_p Set.get sets in
    Lwt.return (n, sets)

  let dances r =
    let%lwt (n, dances) = dances r in
    let%lwt dances = Score.list_map_lwt_p Dance.get dances in
    Lwt.return (n, dances)

  let sources r =
    let%lwt (n, sources) = sources r in
    let%lwt sources = Score.list_map_lwt_p Source.get sources in
    Lwt.return (n, sources)
end

(* * *)

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )
