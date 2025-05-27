open Nes

module Build (Getters : Getters.S) = struct
  include Core.Book

  let get = Getters.get_book

  let short_title book = if short_title book = "" then title book else short_title book
  let short_title' = short_title % Entry.value

  let authors = Lwt_list.map_p Getters.get_person % authors
  let authors' = authors % Entry.value

  let compare : t Entry.t -> t Entry.t -> int =
    Slug.compare_slugs_or
      ~fallback: (fun book1 book2 ->
        (* Compare first by date *)
        let c = compare (Entry.value book1).date (Entry.value book2).date in
        if c = 0 then
          compare book1 book2
        else
          c
      )
      Entry.slug'

  let equal book1 book2 = compare book1 book2 = 0

  let contents book =
    Lwt_list.map_p
      (function
        | Core.Book.Page.Version (version, parameters) ->
          let%lwt version = Getters.get_version version in
          lwt (Version (version, parameters))
        | Core.Book.Page.Set (set, parameters) ->
          let%lwt set = Getters.get_set set in
          lwt (Set (set, parameters))
        | Core.Book.Page.InlineSet (set, parameters) ->
          lwt (InlineSet (set, parameters))
      )
      (contents book)

  let contents' = contents % Entry.value

  let versions_from_contents book =
    let%lwt contents = contents book in
    Lwt_list.filter_map_p
      (function
        | Version (version, _) -> lwt_some version
        | _ -> lwt_none
      )
      contents

  let versions_from_contents' = versions_from_contents % Entry.value

  let isInlineSet = function
    | InlineSet _ -> true
    | _ -> false

  let find_context_no_inline index set =
    let%lwt contents = contents set in
    let contents_no_inline = List.filter (not % isInlineSet) contents in
    lwt @@ List.findi_context (fun i _ -> i = index) contents_no_inline

  let find_context_no_inline' index = find_context_no_inline index % Entry.value

  module BuiltSet = Set.Build(Getters)

  let lilypond_contents_cache_key book =
    let%lwt pages = contents book in
    let%lwt contents =
      Lwt_list.map_p
        (function
          | Version (version, _) -> lwt @@ Core.Version.content' version
          | Set (set, _) -> BuiltSet.lilypond_content_cache_key' set
          | InlineSet (set, _) -> BuiltSet.lilypond_content_cache_key set
        )
        pages
    in
    lwt (String.concat "\n" contents)

  let lilypond_contents_cache_key' = lilypond_contents_cache_key % Entry.value

  let page_to_page_core = function
    | (Version (version, params): page) -> Core.Book.Page.Version (Entry.slug version, params)
    | (Set (set, params): page) -> Core.Book.Page.Set (Entry.slug set, params)
    | (InlineSet (set, params): page) -> Core.Book.Page.InlineSet (set, params)

  let make ~title ?subtitle ?short_title ?authors ?date ?contents ?source ?remark ?scddb_id () =
    let title = String.remove_duplicates ~char: ' ' title in
    let subtitle = Option.map (String.remove_duplicates ~char: ' ') subtitle in
    let short_title = Option.map (String.remove_duplicates ~char: ' ') short_title in
    let authors = Option.map (List.map Entry.slug) authors in
    let contents = Option.map (List.map page_to_page_core) contents in
    make ~title ?subtitle ?short_title ?authors ?date ?contents ?source ?remark ?scddb_id ()

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
          | Version _ | InlineSet _ -> lwt_none
          | Set (set, _) -> lwt_some set
        )
        contents

    let duplicateSet book =
      flip Lwt.map (sets_from_contents' book) @@ fun sets ->
      match List.sort Entry.compare' sets with
      | [] -> []
      | first_set :: other_sets ->
        let (_, warnings) =
          List.fold_left
            (fun (previous_set, warnings) current_set ->
              let warnings =
                if Entry.equal' current_set previous_set then
                    ((DuplicateSet current_set) :: warnings)
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

    let duplicateVersion book =
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
          let%lwt tune = Getters.get_tune @@ Core.Version.tune' v in
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
              let%lwt tune = Getters.get_tune @@ Core.Version.tune' v in
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
                ((DuplicateVersion (tune, set_opts)) :: warnings)
            else
              warnings
          )
          []
      |> lwt

    let sets_and_parameters_from_contents' book =
      let%lwt contents = contents' book in
      Lwt_list.filter_map_p
        (function
          | Version _ | InlineSet _ -> lwt_none
          | Set (set, parameters) -> lwt_some (set, parameters)
        )
        contents

    let setDanceMismatch book =
      let%lwt sets_and_parameters = sets_and_parameters_from_contents' book in
      Lwt_list.filter_map_p
        (fun (set, parameters) ->
          let%olwt dance_slug = lwt (Core.SetParameters.for_dance parameters) in
          (* FIXME: SetParameters should be hidden behind the same kind of
             mechanism as the rest; and this step should not be necessary *)
          let%lwt dance = Getters.get_dance dance_slug in
          if Core.Set.kind' set = Core.Dance.kind' dance then
            lwt_none
          else
            lwt_some (SetDanceMismatch (set, dance))
        )
        sets_and_parameters

    let all book =
      Lwt_list.fold_left_s
        (fun warnings new_warnings_lwt ->
          let%lwt new_warnings = new_warnings_lwt in
          lwt (warnings @ new_warnings)
        )
        []
        [
          empty book;
          duplicateSet book;
          duplicateVersion book;
          setDanceMismatch book
        ]
  end

  let warnings book = Warnings.all book

  let page_core_to_page = function
    | Core.Book.Page.Version (version, params) ->
      let%lwt version = Getters.get_version version in
      lwt @@ Version (version, params)
    | Core.Book.Page.Set (set, params) ->
      let%lwt set = Getters.get_set set in
      lwt @@ Set (set, params)
    | Core.Book.Page.InlineSet (set, params) ->
      lwt @@ InlineSet (set, params)
end
