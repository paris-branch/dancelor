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

  let find_context index set =
    let%lwt versions = List.map fst <$> contents set in
    lwt @@ List.findi_context (fun i _ -> i = index) versions

  let find_context' index = find_context index % Entry.value

  let warnings s =
    let warnings = ref [] in
    let add_warning w = warnings := w :: !warnings in
    let%lwt versions =
      let%lwt contents = contents s in
      lwt (List.map fst contents)
    in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_s (Lwt.map Option.get % Getters.get_tune % Core.Version.tune') versions in
    let tunes = List.sort Entry.compare' tunes in
    (
      match tunes with
      | [] -> add_warning Empty
      | tune :: tunes ->
        let _ =
          List.fold_left
            (fun prev curr ->
              if prev = curr then
                add_warning (Duplicate_tune curr);
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
