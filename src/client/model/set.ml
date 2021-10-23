open Nes
module E = Dancelor_common_model.Set_endpoints
module A = E.Arguments

module Self = struct
  include Dancelor_common_model.Set

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
end
include Self

module Filter = struct
  include Filter

  let accepts filter set =
    let char_equal = Char.Sensible.equal in
    Formula.interpret filter @@ function

    | Is set' ->
      equal set set' >|=| Formula.interpret_bool

    | Name string ->
      let%lwt name = Self.name set in
      Lwt.return (String.proximity ~char_equal string name)

    | NameMatches string ->
      let%lwt name = Self.name set in
      Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

    | Deviser dfilter ->
      (match%lwt Self.deviser set with
       | None -> Lwt.return Formula.interpret_false
       | Some deviser -> Credit.Filter.accepts dfilter deviser)

    | ExistsVersion vfilter ->
      let%lwt versions_and_parameters = Self.versions_and_parameters set in
      Formula.interpret_exists
        (fun (version, _) ->
           Version.Filter.accepts vfilter version)
        versions_and_parameters
end

let get slug =
  Madge_client.(
    call ~endpoint:E.get @@ fun {a} _ ->
    a A.slug slug
  )

let make_and_save ?status ~name ?deviser ~kind ?versions_and_parameters ?dances () =
  Madge_client.(
    call ~endpoint:E.make_and_save @@ fun {a} {o} ->
    o A.status status;
    a A.name name;
    o A.deviser deviser;
    a A.kind kind;
    o A.versions_and_parameters versions_and_parameters;
    o A.dances dances
  )

let delete s =
  let%lwt slug = slug s in (* FIXME: SetDelete could maybe take a set directly? *)
  Madge_client.(
    call ~endpoint:E.delete @@ fun {a} _ ->
    a A.slug slug
  )

let search ?pagination ?threshold filter =
  Madge_client.(
    call ~endpoint:E.search @@ fun {a} {o} ->
    o A.pagination pagination;
    o A.threshold threshold;
    a A.filter filter;
  )

let count filter =
  Madge_client.(
    call ~endpoint:E.count @@ fun {a} _ ->
    a A.filter filter;
  )
