open Nes

module Make (Model : Model_builder.S) = struct
  let char_equal = Char.Sensible.equal

  let rec accepts_user filter user =
    Formula.interpret filter @@ function
      | Core.User.Is user' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.unsafe_equal (Entry.id user) user'
      | Username string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.User.username' user
      | Username_matches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.User.username' user

  and accepts_book filter book =
    Formula.interpret filter @@ function
      | Core.Book.Is book' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id book) book'
      | Title string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.Book.title' book
      | Title_matches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Book.title' book
      | Exists_version vfilter ->
        let%lwt content = Model.Book.contents' book in
        let versions =
          List.concat_map
            (function
              | Model.Book.Versions versions_and_params | Model.Book.Dance (_, Dance_versions versions_and_params) ->
                NEList.(to_list % map fst) versions_and_params
              | _ -> []
            )
            content
        in
        Formula.interpret_exists (accepts_version vfilter) versions
      | Exists_set sfilter ->
        let%lwt content = Model.Book.contents' book in
        let%lwt sets =
          Lwt_list.filter_map_s
            (function
              | Model.Book.Set (s, _) | Model.Book.Dance (_, Dance_set (s, _)) -> lwt_some s
              | _ -> lwt_none
            )
            content
        in
        Formula.interpret_exists (accepts_set sfilter) sets
      | Exists_version_deep vfilter ->
        (* recursive call to check the compound formula *)
        flip accepts_book book @@
          Formula.or_l
            Core.[Formula.pred (Book.Exists_version vfilter);
            Formula.pred (Book.Exists_set (Set.exists_version' vfilter));
            ]
      | Exists_editor pfilter ->
        let%lwt editors = Model.Book.authors' book in
        Formula.interpret_exists (accepts_person pfilter) editors

  and accepts_dance filter dance =
    Formula.interpret filter @@ function
      | Core.Dance.Is dance' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id dance) dance'
      | Name string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.proximity ~char_equal string % NEString.to_string) @@ NEList.to_list @@ Model.Dance.names' dance
      | Name_matches string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.inclusion_proximity ~char_equal ~needle: string % NEString.to_string) @@ NEList.to_list @@ Model.Dance.names' dance
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Dance.kind' dance
      | Exists_deviser pfilter ->
        let%lwt devisers = Model.Dance.devisers' dance in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) devisers in
        lwt (Formula.interpret_or_l scores)

  and accepts_person filter person =
    Formula.interpret filter @@ function
      | Core.Person.Is person' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.unsafe_equal (Entry.id person) person'
      | Name string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.Person.name' person
      | Name_matches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Person.name' person

  and accepts_set filter set =
    Formula.interpret filter @@ function
      | Core.Set.Is set' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id set) set'
      | Name string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.Set.name' set
      | Name_matches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Set.name' set
      | Exists_conceptor pfilter ->
        let%lwt conceptors = Model.Set.conceptors' set in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) conceptors in
        lwt (Formula.interpret_or_l scores)
      | Exists_version vfilter ->
        let%lwt contents = Model.Set.contents' set in
        Formula.interpret_exists (accepts_version vfilter % fst) contents
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Set.kind' set

  and accepts_source filter source =
    Formula.interpret filter @@ function
      | Core.Source.Is source' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.unsafe_equal (Entry.id source) source'
      | Name string ->
        lwt @@
          Formula.interpret_or
            (String.proximity ~char_equal string @@ NEString.to_string @@ Model.Source.name' source)
            (Option.fold ~none: Formula.interpret_false ~some: (String.proximity ~char_equal string % NEString.to_string) (Model.Source.short_name' source))
      | Name_matches string ->
        lwt @@
          Formula.interpret_or
            (String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Source.name' source)
            (Option.fold ~none: Formula.interpret_false ~some: (String.inclusion_proximity ~char_equal ~needle: string % NEString.to_string) (Model.Source.short_name' source))
      | Exists_editor pfilter ->
        let%lwt editors = Model.Source.editors' source in
        Formula.interpret_exists (accepts_person pfilter) editors

  and accepts_tune filter tune =
    Formula.interpret filter @@ function
      | Core.Tune.Is tune' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id tune) tune'
      | Name string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.proximity ~char_equal string % NEString.to_string) @@ NEList.to_list @@ Model.Tune.names' tune
      | Name_matches string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.inclusion_proximity ~char_equal ~needle: string % NEString.to_string) @@ NEList.to_list @@ Model.Tune.names' tune
      | Exists_composer pfilter ->
        let%lwt composers = Model.Tune.composers' tune in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter % Model.Tune.composer_composer) composers in
        lwt (Formula.interpret_or_l scores)
      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ Model.Tune.kind' tune
      | Exists_dance dfilter ->
        let%lwt dances = Model.Tune.dances' tune in
        let%lwt scores = Lwt_list.map_s (accepts_dance dfilter) dances in
        lwt (Formula.interpret_or_l scores)

  and accepts_version filter version =
    Formula.interpret filter @@ function
      | Core.Version.Is version' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id version) version'
      | Tune tfilter ->
        let%lwt tune = Model.Version.tune' version in
        accepts_tune tfilter tune
      | Key key' ->
        lwt @@ Formula.interpret_bool (Model.Version.key' version = key')
      | Exists_source sfilter ->
        let%lwt sources = List.map (fun {Model.Version.source; _} -> source) <$> Model.Version.sources' version in
        Formula.interpret_exists (accepts_source sfilter) sources
end
