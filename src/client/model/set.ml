open Nes
include Dancelor_common_model.Set

let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)
let versions = versions >=>| Lwt_list.map_p Version.get

let warnings s =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in
  (* Check that version kinds and bars correspond to set's kind. *)
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
  let%lwt versions = versions s in
  let%lwt () =
    Lwt_list.iter_s
      (fun version ->
         let%lwt version_bars = Version.bars version in
         if version_bars <> bars then
           add_warning (WrongVersionBars version);
         let%lwt tune = Version.group version in
         let%lwt version_kind = Tune.kind tune in
         if version_kind <> kind then
           add_warning (WrongVersionKind tune);
         Lwt.return ())
      versions
  in
  (* Check that there are no duplicates. *)
  let%lwt tunes = Lwt_list.map_s Version.group versions in
  let tunes = List.sort compare tunes in
  (match tunes with
   | [] -> add_warning Empty
   | tune :: tunes ->
     let _ =
       List.fold_left
         (fun prev curr ->
            if prev = curr then
              add_warning (DuplicateVersion curr);
            curr)
         tune
         tunes
     in
     ());
  Lwt.return !warnings

(* * *)

let get slug =
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let all ?pagination () =
  Madge_client.(
    call ~endpoint:Endpoint.all @@ fun _ {o} ->
    o Arg.pagination pagination
  )

let make_and_save ?status ~name ?deviser ~kind ?versions () =
  Madge_client.(
    call ~endpoint:Endpoint.make_and_save @@ fun {a} {o} ->
    o Arg.status status;
    a Arg.name name;
    o Arg.deviser deviser;
    a Arg.kind kind;
    o Arg.versions versions
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

let count () =
  Madge_client.call ~endpoint:Endpoint.count @@ fun _ _ -> ()
