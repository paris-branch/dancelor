open Nes

module Build (Getters : Getters.S) = struct
  include Core.Book

  let get = Getters.get_book

  let authors = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % authors
  let authors' = authors % Entry.value_private_

  let sources = Lwt_list.map_p (Lwt.map Option.get % Getters.get_source) % sources
  let sources' = sources % Entry.value_private_

  let contents book =
    Lwt_list.map_p
      (function
        | Core.Book.Page.Part title -> lwt (Part title)
        | Core.Book.Page.Dance (dance, page_dance) ->
          let%lwt dance = Option.get <$> Getters.get_dance dance in
          let%lwt page_dance =
            match page_dance with
            | Core.Book.Page.Dance_only -> lwt Dance_only
            | Core.Book.Page.Dance_versions versions_and_params ->
              let%lwt versions_and_params = NEList.map_lwt_p (Pair.map_fst_lwt (Option.get <%> Getters.get_version)) versions_and_params in
              lwt @@ Dance_versions versions_and_params
            | Core.Book.Page.Dance_set (set, parameters) ->
              let%lwt set = Option.get <$> Getters.get_set set in
              lwt @@ Dance_set (set, parameters)
          in
          lwt (Dance (dance, page_dance))
        | Core.Book.Page.Versions versions_and_params ->
          let%lwt versions_and_params = NEList.map_lwt_p (Pair.map_fst_lwt (Option.get <%> Getters.get_version)) versions_and_params in
          lwt (Versions versions_and_params)
        | Core.Book.Page.Set (set, parameters) ->
          let%lwt set = Option.get <$> Getters.get_set set in
          lwt (Set (set, parameters))
      )
      (contents book)

  let contents' = contents % Entry.value_private_

  let versions_from_contents book =
    let%lwt contents = contents book in
    lwt @@
      List.concat_map
        (function
          | Versions versions_and_params -> NEList.(to_list % map fst) versions_and_params
          | _ -> []
        )
        contents

  let versions_from_contents' = versions_from_contents % Entry.value_private_

  module BuiltSet = Set.Build(Getters)

  module Warnings = struct
    (* The following functions all have the name of a warning of
       {!Dancelor_common.Model.Core.Book.warning}. They all are in charge of
       generating a list of the associated warning corresponding to the given
       book. The {!all} function then gathers all these warnings in a common list. *)

    let empty book =
      let%lwt contents = contents' book in
      if contents = [] then
        lwt [Empty]
      else
        lwt_nil

    let sets_from_contents' book =
      let%lwt contents = contents' book in
      Lwt_list.filter_map_p
        (function
          | Part _
          | Dance (_, Dance_only)
          | Dance (_, Dance_versions _)
          | Versions _ ->
            lwt_none
          | Dance (_, Dance_set (set, _))
          | Set (set, _) ->
            lwt_some set
        )
        contents

    let duplicate_set book =
      flip Lwt.map (sets_from_contents' book) @@ fun sets ->
      match List.sort Entry.compare' sets with
      | [] -> []
      | first_set :: other_sets ->
        let (_, warnings) =
          List.fold_left
            (fun (previous_set, warnings) current_set ->
              let warnings =
                if Entry.equal' current_set previous_set then
                    ((Duplicate_set current_set) :: warnings)
                else
                  warnings
              in
                (current_set, warnings)
            )
            (first_set, [])
            other_sets
        in
        warnings

    let unique_sets_from_contents' =
      Lwt.map (List.sort_uniq Entry.compare') % sets_from_contents'

    let duplicate_tune book =
      let%lwt sets = unique_sets_from_contents' book in
      let%lwt standalone_versions = versions_from_contents' book in
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
          let%lwt tune = Option.get <$> Getters.get_tune @@ Core.Version.tune' v in
          register_tune_to_set tune None;
          lwt_unit
        )
        standalone_versions;%lwt
      (* register tunes in sets *)
      Lwt_list.iter_s
        (fun set ->
          let%lwt contents = BuiltSet.contents' set in
          let versions = List.map fst contents in
          Lwt_list.iter_s
            (fun v ->
              let%lwt tune = Option.get <$> Getters.get_tune @@ Core.Version.tune' v in
              register_tune_to_set tune (Some set);
              lwt_unit
            )
            versions
        )
        sets;%lwt
      (* crawl all registered tunes and see if they appear several times. if that is
         the case, add a warning accordingly *)
      Hashtbl.to_seq tunes_to_sets
      |> List.of_seq
      |> List.fold_left
          (fun warnings (tune, set_opts) ->
            let set_opts = List.sort_count (Option.compare Entry.compare') set_opts in
            if List.length set_opts > 1 then
                ((Duplicate_tune (tune, set_opts)) :: warnings)
            else
              warnings
          )
          []
      |> lwt

    let set_dance_kind_mismatch book =
      let%lwt contents = contents' book in
      Lwt_list.filter_map_s
        (function
          | Dance (dance, Dance_set (set, _)) ->
            if Core.Dance.kind' dance <> Core.Set.kind' set then
              lwt_some (Set_dance_kind_mismatch (set, dance))
            else
              lwt_none
          | _ -> lwt_none
        )
        contents

    let all book =
      Lwt_list.fold_left_s
        (fun warnings new_warnings_lwt ->
          let%lwt new_warnings = new_warnings_lwt in
          lwt (warnings @ new_warnings)
        )
        []
        [
          empty book;
          duplicate_set book;
          duplicate_tune book;
          set_dance_kind_mismatch book;
        ]
  end

  let warnings book = Warnings.all book

  let page_core_to_page = function
    | Core.Book.Page.Part title -> lwt (Part title)
    | Core.Book.Page.Dance (dance, page_dance) ->
      let%lwt dance = Option.get <$> Getters.get_dance dance in
      let%lwt page_dance =
        match page_dance with
        | Core.Book.Page.Dance_only -> lwt Dance_only
        | Core.Book.Page.Dance_versions versions_and_params ->
          let%lwt versions_and_params = NEList.map_lwt_p (Pair.map_fst_lwt (Option.get <%> Getters.get_version)) versions_and_params in
          lwt @@ Dance_versions versions_and_params
        | Core.Book.Page.Dance_set (set, params) ->
          let%lwt set = Option.get <$> Getters.get_set set in
          lwt @@ Dance_set (set, params)
      in
      lwt @@ Dance (dance, page_dance)
    | Core.Book.Page.Versions versions_and_params ->
      let%lwt versions_and_params = NEList.map_lwt_p (Pair.map_fst_lwt (Option.get <%> Getters.get_version)) versions_and_params in
      lwt @@ Versions versions_and_params
    | Core.Book.Page.Set (set, params) ->
      let%lwt set = Option.get <$> Getters.get_set set in
      lwt @@ Set (set, params)
end
