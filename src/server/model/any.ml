open Nes

include Dancelor_common_model.Any

module Filter = struct
  include Filter

  let accepts filter any =
    Formula.interpret filter @@ function
    | Type type_ ->
      Type.equal (type_of any) type_
      |> Formula.interpret_bool
      |> Lwt.return

    | AsCredit cfilter ->
      (match any with
       | Credit credit -> Credit.Filter.accepts cfilter credit
       | _ -> Lwt.return Formula.interpret_false)

    | AsDance dfilter ->
      (match any with
       | Dance dance -> Dance.Filter.accepts dfilter dance
       | _ -> Lwt.return Formula.interpret_false)

    | AsPerson pfilter ->
      (match any with
       | Person person -> Person.Filter.accepts pfilter person
       | _ -> Lwt.return Formula.interpret_false)

    | AsBook bfilter ->
      (match any with
       | Book book -> Book.Filter.accepts bfilter book
       | _ -> Lwt.return Formula.interpret_false)

    | AsSet sfilter ->
      (match any with
       | Set set -> Set.Filter.accepts sfilter set
       | _ -> Lwt.return Formula.interpret_false)

    (* | AsSource sfilter -> *)
    (*   (match any with *)
    (*    | Source source -> Source.Filter.accepts sfilter source *)
    (*    | _ -> Lwt.return Formula.interpret_false) *)

    | AsTune tfilter ->
      (match any with
       | Tune tune -> Tune.Filter.accepts tfilter tune
       | _ -> Lwt.return Formula.interpret_false)

    | AsVersion vfilter ->
      (match any with
       | Version version -> Version.Filter.accepts vfilter version
       | _ -> Lwt.return Formula.interpret_false)
end

module E = Dancelor_common_model.Any_endpoints
module A = E.Arguments

let search ?pagination ?(threshold=0.) filter =
  let%lwt credits  = Dancelor_server_database.Credit.get_all ()  >|=| List.map (fun c -> Credit c) in
  let%lwt dances   = Dancelor_server_database.Dance.get_all ()   >|=| List.map (fun d -> Dance d) in
  let%lwt persons  = Dancelor_server_database.Person.get_all ()  >|=| List.map (fun p -> Person p) in
  let%lwt books    = Dancelor_server_database.Book.get_all ()    >|=| List.map (fun b -> Book b) in
  let%lwt sets     = Dancelor_server_database.Set.get_all ()     >|=| List.map (fun s -> Set s) in
  (* let%lwt sources  = Dancelor_server_database.Source.get_all ()  >|=| List.map (fun s -> Source s) in *)
  let%lwt tunes    = Dancelor_server_database.Tune.get_all ()    >|=| List.map (fun t -> Tune t) in
  let%lwt versions = Dancelor_server_database.Version.get_all () >|=| List.map (fun v -> Version v) in
  (Lwt.return (credits @ dances @ persons @ books @ sets @ tunes @ versions))
  >>=| Score.lwt_map_from_list (Filter.accepts filter)
  >>=| (Score.list_filter_threshold threshold ||> Lwt.return)
  >>=| Score.(list_proj_sort_decreasing [])
  >>=| Option.unwrap_map_or ~default:Lwt.return Pagination.apply pagination

let () =
  Madge_server.(
    register ~endpoint:E.search @@ fun {a} {o} ->
    search
      ?pagination:(o A.pagination)
      ?threshold:(o A.threshold)
      (a A.filter)
  )
