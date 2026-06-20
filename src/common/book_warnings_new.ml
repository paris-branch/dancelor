open Nes
open Model_builder.Core.Book
open Model_new

module Build (Getters : Model_builder.Getters) = struct
  module Built_set = Model_builder.Builder.Set.Build(Getters)

  (* The following functions all have the name of a warning of
     {!Dancelor_common.Model.Core.Book.warning}. They all are in charge of
     generating a list of the associated warning corresponding to the given
     book. The {!all} function then gathers all these warnings in a common list. *)

  let empty (book : Book_view.t) = if book.content = [] then [Empty] else []

  let versions_from_content (book : Book_view.t) =
    List.concat_map
      (function
        | Versions versions_and_params -> NEList.(to_list % map fst) versions_and_params
        | _ -> []
      )
      book.content

  let sets_from_content (book : Book_view.t) =
    List.filter_map
      (function
        | Part _
        | Dance (_, Dance_only)
        | Dance (_, Dance_versions _)
        | Versions _ ->
          None
        | Dance (_, Dance_set (set, _))
        | Set (set, _) ->
          Some set
      )
      book.content

  let duplicate_set book =
    let sets = sets_from_content book in
    match List.sort Entry.Id.compare' sets with
    | [] -> []
    | first_set :: other_sets ->
      let (_, warnings) =
        List.fold_left
          (fun (previous_set, warnings) current_set ->
            let warnings =
              if Entry.Id.equal' current_set previous_set then
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

  let unique_sets_from_content =
    List.sort_uniq Entry.Id.compare' % sets_from_content

  let duplicate_tune book =
    let sets = unique_sets_from_content book in
    let standalone_versions = versions_from_content book in
    (* [tunes_to_sets] is a hashtable from tunes to sets they belong to.
       Standalone tunes are associated with None *)
    let tunes_to_sets = Hashtbl.create 8 in
    (* register standalone tunes *)
    Lwt_list.iter_s
      (fun v ->
        let%lwt tune = (Model_builder.Core.Version.tune' % Option.get) <$> Getters.get_version v in
        Hashtbl.add tunes_to_sets tune None;
        lwt_unit
      )
      standalone_versions;%lwt
    (* register tunes in sets *)
    Lwt_list.iter_s
      (fun set ->
        let%lwt versions = (List.map fst % Built_set.contents' % Option.get) <$> Getters.get_set set in
        Lwt_list.iter_s
          (fun v ->
            let%lwt tune = (Model_builder.Core.Version.tune' % Option.get) <$> Getters.get_version v in
            Hashtbl.add tunes_to_sets tune (Some set);
            lwt_unit
          )
          versions
      )
      sets;%lwt
    (* crawl all registered tunes and see if they appear several times. if that is
       the case, add a warning accordingly *)
    Hashtbl.to_seq_keys tunes_to_sets
    |> List.of_seq
    |> List.fold_left
        (fun warnings tune ->
          let set_opts = List.sort_count (Option.compare Entry.Id.compare') (Hashtbl.find_all tunes_to_sets tune) in
          if List.length set_opts > 1 then
              ((Duplicate_tune (tune, set_opts)) :: warnings)
          else
            warnings
        )
        []
    |> lwt

  let set_dance_kind_mismatch (book : Book_view.t) =
    Lwt_list.filter_map_s
      (function
        | Dance (dance, Dance_set (set, _)) ->
          let%lwt dance = Option.get <$> Getters.get_dance dance in
          let%lwt set = Option.get <$> Getters.get_set set in
          if Model_builder.Core.Dance.kind' dance <> Model_builder.Core.Set.kind' set then
            lwt_some (Set_dance_kind_mismatch (Entry.id set, Entry.id dance))
          else
            lwt_none
        | _ -> lwt_none
      )
      book.content

  let all book =
    Lwt_list.fold_left_s
      (fun warnings new_warnings_lwt ->
        let%lwt new_warnings = new_warnings_lwt in
        lwt (warnings @ new_warnings)
      )
      []
      [
        (lwt @@ empty book);
        (lwt @@ duplicate_set book);
        duplicate_tune book;
        set_dance_kind_mismatch book;
      ]
end
