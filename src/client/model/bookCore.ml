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

module Warnings = struct
  let empty dance =
    let%lwt contents = contents dance in
    if contents = [] then
      Lwt.return [Empty]
    else
      Lwt.return_nil

  let standalone_versions dance =
    let%lwt contents = contents dance in
    Lwt_list.filter_map_p
      (function
        | Version (v, _) -> Lwt.return_some v
        | _ -> Lwt.return_none)
      contents

  let sets dance =
    let%lwt contents = contents dance in
    Lwt_list.filter_map_p
      (function
        | Version _ -> Lwt.return_none
        | Set (s, _) | InlineSet (s, _) -> Lwt.return_some s)
      contents

  let unique_sets dance =
    let%lwt sets = sets dance in
    List.sort_uniq_lwt Set.compare sets

  let sets_and_parameters dance =
    let%lwt contents = contents dance in
    Lwt_list.filter_map_p
      (function
        | Set (set, parameters) | InlineSet (set, parameters) ->
          Lwt.return_some (set, parameters)
        | Version _ -> Lwt.return_none)
      contents

  let duplicateSet dance =
    let%lwt sets = sets dance in
    match%lwt List.sort_lwt Set.compare sets with
    | [] -> Lwt.return_nil
    | first_set :: other_sets ->
      let%lwt (_, warnings) =
        Lwt_list.fold_left_s
          (fun (previous_set, warnings) current_set ->
             let%lwt warnings =
               if%lwt Set.equal current_set previous_set then
                 Lwt.return ((DuplicateSet current_set) :: warnings)
               else
                 Lwt.return warnings
             in
             Lwt.return (current_set, warnings))
          (first_set, [])
          other_sets
      in
      Lwt.return warnings

  let duplicateVersion dance =
    let%lwt sets = unique_sets dance in
    let%lwt standalone_versions = standalone_versions dance in
    (* [tunes_to_sets] is a hashtable from tunes to sets they belong to.
       Standalone tunes are associated with None *)
    let tunes_to_sets = Hashtbl.create 8 in
    (* Extend the list of sets associated to this tune. Creates it if it was not
       yet in the hashtable *)
    let register_tune_to_set tune set_opt =
      match Hashtbl.find_opt tunes_to_sets tune with
      | None -> Hashtbl.add tunes_to_sets tune [set_opt]
      | Some set_opts -> Hashtbl.replace tunes_to_sets tune (set_opt :: set_opts)
    in
    (* register standalone tunes *)
    Lwt_list.iter_s
      (fun v ->
         let%lwt tune = Version.tune v in
         register_tune_to_set tune None;
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
              register_tune_to_set tune (Some set);
              Lwt.return ())
           versions)
      sets;%lwt
    (* crawl all registered tunes and see if they appear several times. if that is
       the case, add a warning accordingly *)
    Hashtbl.to_seq tunes_to_sets
    |> List.of_seq
    |> Lwt_list.fold_left_s
      (fun warnings (tune, set_opts) ->
         let%lwt set_opts = List.sort_count_lwt (Option.compare_lwt Set.compare) set_opts in
         if List.length set_opts > 1 then
           Lwt.return ((DuplicateVersion (tune, set_opts)) :: warnings)
         else
           Lwt.return warnings)
      []

  let setDanceMismatch dance =
    let%lwt sets_and_parameters = sets_and_parameters dance in
    Lwt_list.filter_map_p
      (fun (set, parameters) ->
         match SetParameters.for_dance parameters with
         | None -> Lwt.return_none
         | Some dance_slug ->
           (* FIXME: SetParameters should be hidden behind the same kind of
              mechanism as the rest; and this step should not be necessary *)
           let%lwt dance = Dance.get dance_slug in
           let%lwt dance_kind = DanceCore.kind dance in
           let%lwt set_kind = SetCore.kind set in
           if set_kind = dance_kind then
             Lwt.return_none
           else
             Lwt.return_some (SetDanceMismatch (set, dance)))
      sets_and_parameters

  let all dance =
    Lwt_list.fold_left_s
      (fun warnings new_warnings_lwt ->
         let%lwt new_warnings = new_warnings_lwt in
         Lwt.return (warnings @ new_warnings))
      []
      [ empty dance;
        duplicateSet dance;
        duplicateVersion dance;
        setDanceMismatch dance ]
end

let warnings dance = Warnings.all dance

let get the_slug =
  let open BookEndpoints in
  let open Arguments in
  Madge_client.(
    call ~endpoint:get @@ fun {a} _ ->
    a slug the_slug
  )
