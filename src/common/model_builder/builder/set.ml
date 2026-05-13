open Nes

module Build (Getters : Getters.S) = struct
  include Core.Set

  let get = Getters.get_set

  let warnings s =
    let warnings = ref [] in
    let add_warning w = warnings := w :: !warnings in
    let%lwt versions = Lwt_list.map_p (Option.get <%> Getters.get_version % fst) (contents s) in
    (* Check that there are no duplicates. *)
    let%lwt tunes = Lwt_list.map_p (Option.get <%> Getters.get_tune % Core.Version.tune') versions in
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

  let warnings' = warnings % Entry.value_private_
end
