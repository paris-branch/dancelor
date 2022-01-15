open Nes
open Dancelor_common_model
include BookCore

let contents p =
  let%lwt contents = contents p in
  Lwt_list.map_p
    (function
      | (Version (v, p) : page_slug) -> let%lwt v = Version.get v in Lwt.return (Version (v, p))
      | Set (s, p) -> let%lwt s = Set.get s in Lwt.return (Set (s, p))
      | InlineSet (s, p) -> Lwt.return (InlineSet (s, p)))
    contents

let warnings p =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in

  let%lwt contents = contents p in

  (* Raise the [Empty] warning if there is nothing in this book *)
  if contents = [] then add_warning Empty;

  (* Check that there are no duplicate sets. *)
  let%lwt sets =
    Lwt_list.filter_map_p
      (function
        | Version _ -> Lwt.return_none
        | Set (s, _) | InlineSet (s, _) -> Lwt.return_some s)
      contents
  in
  let%lwt sets = List.sort_lwt Set.compare sets in
  (match sets with
   | [] -> ()
   | set :: sets ->
     let _ =
       List.fold_left
         (fun prev curr ->
            if prev = curr then
              add_warning (DuplicateSet curr);
            curr)
         set sets
     in
     ());

  (* remove duplicate sets to avoid further warnings *)
  (* FIXME: we know that [sets] is sorted so we could use something more
     efficient here *)
  let%lwt sets = List.sort_uniq_lwt Set.compare sets in

  (* Check that there are no duplicate tune. *)
  let%lwt standalone_versions =
    Lwt_list.filter_map_p
      (function
        | Version (v, _) -> Lwt.return_some v
        | _ -> Lwt.return_none)
      contents
  in

  (* Extend the list of sets associated to this tune. Creates it if it was not yet in the hashtable *)
  let extend htbl tune set_opt =
    match Hashtbl.find_opt htbl tune with
    | None -> Hashtbl.add htbl tune [set_opt]
    | Some sets -> Hashtbl.replace htbl tune (set_opt :: sets)
  in

  (* [tunes_to_set] is a hashtable from tunes to sets they belong to.
     Standalone tunes are associated with None *)
  let tunes_to_set = Hashtbl.create 8 in
  (* register standalone tunes *)
  Lwt_list.iter_s
    (fun v ->
       let%lwt tune = Version.tune v in
       extend tunes_to_set tune None;
       Lwt.return ())
    standalone_versions;%lwt
  (* register tunes in sets *)
  Lwt_list.iter_s
    (fun set ->
       let%lwt versions_and_parameters = Set.versions_and_parameters set in
       let versions = List.map fst versions_and_parameters in
       Lwt_list.iter_s
         (fun v ->
            let%lwt tune = Version.tune v in
            extend tunes_to_set tune (Some set);
            Lwt.return ())
         versions)
    sets;%lwt
  (* crawl all registered tunes and see if they appear several times. if that is
     the case, add a warning accordingly *)
  Hashtbl.to_seq tunes_to_set
  |> Seq.iter_lwt
    (fun (tune, sets_opt) ->
       let%lwt sets_opt = List.sort_count_lwt (Option.compare_lwt Set.compare) sets_opt in
       if List.length sets_opt > 1 then
          add_warning (DuplicateVersion (tune, sets_opt));
       Lwt.return_unit);%lwt

  (* Check if a set points to a dance with a different kind *)
  Lwt_list.iter_s
    (function
    | Set (set, p) | InlineSet (set, p) ->
      let dance = SetParameters.for_dance p in
      (match dance with
      | None -> Lwt.return_unit
      | Some dance_slug ->
        let%lwt dance = Dance.get dance_slug in
        let%lwt dance_kind = DanceCore.kind dance in
        let%lwt set_kind = SetCore.kind set in
        if set_kind = dance_kind then Lwt.return_unit
        else (add_warning (SetDanceMismatch (set, dance)); Lwt.return_unit)
      )
    | Version _ -> Lwt.return_unit
    ) contents;%lwt

  (* Return *)
  Lwt.return !warnings

let get the_slug =
  let open BookEndpoints in
  let open Arguments in
  Madge_client.(
    call ~endpoint:get @@ fun {a} _ ->
    a slug the_slug
  )
