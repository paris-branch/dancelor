include Dancelor_common_model.Set

let deviser s =
  match%lwt deviser s with
  | None -> Lwt.return_none
  | Some slug ->
    let%lwt c =
      Dancelor_client_api.request
        ~route:(Dancelor_common.Router.Credit slug)
        ~reader:Credit.of_yojson
        ()
    in
    Lwt.return_some c

let tunes s =
  let%lwt tunes = tunes s in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Tune slug)
         ~reader:Tune.of_yojson
         ())
    tunes

let warnings s =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in
  (* Check that tune kinds and bars correspond to set's kind. *)
  let%lwt (bars, kind) =
    match%lwt kind s with
    | (_, []) ->
      add_warning WrongKind;
      Lwt.return (32, Kind.Reel) (* FIXME *)
    | (_, [(bars, kind)]) ->
      Lwt.return (bars, kind)
    | (_, (bars, kind) :: _) ->
      (* FIXME: more complicated that it appears *)
      Lwt.return (bars, kind)
  in
  let%lwt tunes = tunes s in
  let%lwt () =
    Lwt_list.iter_s
      (fun tune ->
         let%lwt tune_bars = Tune.bars tune in
         if tune_bars <> bars then
           add_warning (WrongTuneBars tune);
         let%lwt tune_group = Tune.group tune in
         let%lwt tune_kind = TuneGroup.kind tune_group in
         if tune_kind <> kind then
           add_warning (WrongTuneKind tune_group);
         Lwt.return ())
      tunes
  in
  (* Check that there are no duplicates. *)
  let%lwt tune_groups = Lwt_list.map_s Tune.group tunes in
  let tune_groups = List.sort compare tune_groups in
  (match tune_groups with
   | [] -> add_warning Empty
   | tune_group :: tune_groups ->
     let _ =
       List.fold_left
         (fun prev curr ->
            if prev = curr then
              add_warning (DuplicateTune curr);
            curr)
         tune_group
         tune_groups
     in
     ());  
  Lwt.return !warnings

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Set slug)
    ~reader:of_yojson
    ()

let get_all () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.SetAll
    ~reader:(Dancelor_common.Unserializer.list of_yojson)
    ()

let make_and_save ~name ?deviser ~kind ?status ?tunes () =
  let%lwt tunes =
    match tunes with
    | None -> Lwt.return_none
    | Some tunes ->
      let%lwt tunes = Lwt_list.map_s Tune.slug tunes in
      Lwt.return_some tunes
  in
  let%lwt deviser =
    match deviser with
    | None -> Lwt.return []
    | Some deviser ->
      let%lwt deviser = Credit.slug deviser in
      Lwt.return [deviser]
  in
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.SetSave
    ~reader:of_yojson
    ~query:(
      ["name", [name]]
      @ ["deviser", deviser]
      @ ["kind", [Kind.dance_to_string kind]]
      @ (match status with None -> [] | Some status -> ["status", [Status.to_string status]])
      @ (match tunes with None -> [] | Some tunes -> ["tunes", tunes])
    )
    ()

let delete s =
  let%lwt slug = slug s in (* FIXME: SetDelete could maybe take a set directly? *)
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.SetDelete slug)
    ~reader:Dancelor_common.Unserializer.unit
    ()
