open Nes

module Build (Getters : Getters.S) = struct
  include Core.Set

  let get = Getters.get_set

  let conceptors = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % conceptors
  let conceptors' = conceptors % Entry.value

  let dances = Lwt_list.map_p (Lwt.map Option.get % Getters.get_dance) % dances
  let dances' = dances % Entry.value

  let contents =
    Lwt_list.map_s
      (fun (id, parameters) ->
        let%lwt version = Option.get <$> Getters.get_version id in
        lwt (version, parameters)
      ) %
      contents
  let contents' = contents % Entry.value

  let compare : t Entry.t -> t Entry.t -> int = fun x y ->
    Entry.Id.compare' (Entry.id' x) (Entry.id' y)
  let equal set1 set2 = compare set1 set2 = 0

  (* FIXME: use Version.equal *)
  let contains_version id1 set =
    List.exists
      (fun (id2, _parameters) ->
        Entry.Id.equal' id1 id2
      )
      (Entry.value set).contents

  let find_context index set =
    let%lwt versions = List.map fst <$> contents set in
    lwt @@ List.findi_context (fun i _ -> i = index) versions

  let find_context' index = find_context index % Entry.value

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
      lwt (List.map fst contents)
    in
    let%lwt () =
      Lwt_list.iter_s
        (fun version ->
          if Core.Version.bars' version <> bars then
            add_warning (WrongVersionBars version);
          let%lwt tune = Option.get <$> Getters.get_tune @@ Core.Version.tune' version in
          if Core.Tune.kind' tune <> kind then
            add_warning (WrongVersionKind tune);
          lwt_unit
        )
        versions
    in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_s (Lwt.map Option.get % Getters.get_tune % Core.Version.tune') versions in
    let tunes = List.sort Core.Tune.compare tunes in
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
    lwt !warnings

  let warnings' = warnings % Entry.value
end
