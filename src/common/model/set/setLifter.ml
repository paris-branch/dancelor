open Nes

module Lift
    (Credit  : module type of  CreditSignature)
    (Dance   : module type of   DanceSignature)
    (Tune    : module type of    TuneSignature)
    (Version : module type of VersionSignature)
= struct
  include SetCore

  let deviser = deviser >=>?| (Credit.get >=>| Lwt.return_some)

  let versions_and_parameters set =
    let%lwt versions_and_parameters = versions_and_parameters set in
    Lwt_list.map_s
      (fun (slug, parameters) ->
         let%lwt version = Version.get slug in
         Lwt.return (version, parameters))
      versions_and_parameters

  let dances = dances >=>| Lwt_list.map_p Dance.get

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
    let%lwt versions =
      let%lwt versions_and_parameters = versions_and_parameters s in
      Lwt.return (List.map fst versions_and_parameters)
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun version ->
           let%lwt version_bars = Version.bars version in
           if version_bars <> bars then
             add_warning (WrongVersionBars version);
           let%lwt tune = Version.tune version in
           let%lwt version_kind = Tune.kind tune in
           if version_kind <> kind then
             add_warning (WrongVersionKind tune);
           Lwt.return ())
        versions
    in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_s Version.tune versions in
    let%lwt tunes = List.sort_lwt Tune.compare tunes in
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
end
