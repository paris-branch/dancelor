open Nes

module Build
    (Dance : Signature.Dance.S)
    (Person : Signature.Person.S)
    (Tune : Signature.Tune.S)
    (Version : Signature.Version.S)
= struct
  include Core.Set

  let make
      ~name
      ?conceptors
      ~kind
      ?contents
      ~order
      ?dances
      ()
    =
    let name = String.remove_duplicates ~char: ' ' name in
    let conceptors = Option.map (List.map Entry.slug) conceptors in
    let contents = Option.map (List.map (fun (version, parameters) -> (Entry.slug version, parameters))) contents in
    let dances = Option.map (List.map Entry.slug) dances in
    make ~name ?conceptors ~kind ?contents ~order ?dances ()

  let is_slug_none : t Entry.t -> bool = Slug.is_none % Entry.slug
  let conceptors = Lwt_list.map_p Person.get % conceptors
  let dances = Lwt_list.map_p Dance.get % dances

  let contents =
    Lwt_list.map_s
      (fun (slug, parameters) ->
         let%lwt version = Version.get slug in
         Lwt.return (version, parameters)
      ) %
    contents

  let compare : t Entry.t -> t Entry.t -> int =
    Slug.compare_slugs_or ~fallback: Stdlib.compare Entry.slug'
  let equal set1 set2 = compare set1 set2 = 0

  (* FIXME: use Version.equal *)
  let contains_version slug1 set =
    List.exists
      (fun (slug2, _parameters) ->
         Slug.equal' slug1 slug2
      )
      (Entry.value set).contents

  let find_context index set =
    let%lwt versions = Lwt.map (List.map fst) @@ contents set in
    Lwt.return @@ List.findi_context (fun i _ -> i = index) versions

  let lilypond_content_cache_key set =
    let%lwt contents = contents set in
    let contents = List.map (Version.content % fst) contents in
    Lwt.return (String.concat "\n" contents)

  let warnings s =
    let warnings = ref [] in
    let add_warning w = warnings := w :: !warnings in
    (* Check that version kinds and bars correspond to set's kind. *)
    let (bars, kind) =
      match kind s with
      | Mul (_, Version (bars, kind)) -> (bars, kind)
      | _ ->
        (* FIXME: more complicated that it appears: For sets that have a medley
           kind, checking that “the version has a compatible kind” makes little
           sense. I don't think there is a very good solution right now; ideally
           later we should check that the versions “added” as per the set's
           order sum up to the kind of the set, but that's more involved. *)
        (32, KindBase.Reel)
    in
    let%lwt versions =
      let%lwt contents = contents s in
      Lwt.return (List.map fst contents)
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun version ->
           if Version.bars version <> bars then
             add_warning (WrongVersionBars version);
           let%lwt tune = Version.tune version in
           if Tune.kind tune <> kind then
             add_warning (WrongVersionKind tune);
           Lwt.return ()
        )
        versions
    in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_s Version.tune versions in
    let tunes = List.sort Tune.compare tunes in
    (
      match tunes with
      | [] -> add_warning Empty
      | tune :: tunes ->
        let _ =
          List.fold_left
            (fun prev curr ->
               if prev = curr then
                 add_warning (DuplicateVersion curr);
               curr
            )
            tune
            tunes
        in
        ()
    );
    Lwt.return !warnings

  module Filter = struct
    include Filter.Set

    let accepts filter set =
      let char_equal = Char.Sensible.equal in
      Formula.interpret filter @@ function
      | Is set' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug set) set'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Core.Set.name set
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Core.Set.name set
      | ExistsConceptor pfilter ->
        let%lwt conceptors = conceptors set in
        let%lwt scores = Lwt_list.map_s (Person.Filter.accepts pfilter) conceptors in
        Lwt.return (Formula.interpret_or_l scores)
      | ExistsVersion vfilter ->
        let%lwt contents = contents set in
        Formula.interpret_exists
          (fun (version, _) ->
             Version.Filter.accepts vfilter version
          )
          contents
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Core.Set.kind set
  end
end
