open Nes
include Dancelor_common_model.Set

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)
let tunes = tunes >=>| Lwt_list.map_p Tune.get

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
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let get_all () =
  Madge_client.call ~endpoint:Endpoint.get_all @@ fun _ _ -> ()

let make_and_save ?status ~name ?deviser ~kind ?tunes () =
  Madge_client.(
    call ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    o Arg.status status;
    a Arg.name name;
    o Arg.deviser deviser;
    a Arg.kind kind;
    o Arg.tunes tunes
  )

let delete s =
  let%lwt slug = slug s in (* FIXME: SetDelete could maybe take a set directly? *)
  Madge_client.(
    call ~endpoint:Endpoint.delete @@ fun {a} _ ->
    a Arg.slug slug
  )

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )
