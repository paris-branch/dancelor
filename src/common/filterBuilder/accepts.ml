open Nes

module Make (Model : ModelBuilder.S) = struct
  let char_equal = Char.Sensible.equal

  let rec accepts_book filter book =
    Formula.interpret filter @@ function
      | Core.Book.Is book' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id book) book'
      | Title string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.Book.title' book
      | TitleMatches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Book.title' book
      | ExistsVersion vfilter ->
        let%lwt content = Model.Book.contents' book in
        let versions =
          List.concat_map
            (function
              | Model.Book.Versions versions_and_params | Model.Book.Dance (_, DanceVersions versions_and_params) ->
                NEList.(to_list % map fst) versions_and_params
              | _ -> []
            )
            content
        in
        Formula.interpret_exists (accepts_version vfilter) versions
      | ExistsSet sfilter ->
        let%lwt content = Model.Book.contents' book in
        let%lwt sets =
          Lwt_list.filter_map_s
            (function
              | Model.Book.Set (s, _) | Model.Book.Dance (_, DanceSet (s, _)) -> lwt_some s
              | _ -> lwt_none
            )
            content
        in
        Formula.interpret_exists (accepts_set sfilter) sets
      | ExistsVersionDeep vfilter ->
        (* recursive call to check the compound formula *)
        flip accepts_book book @@
          Formula.or_l
            Core.[Formula.pred (Book.ExistsVersion vfilter);
            Formula.pred (Book.ExistsSet (Set.existsversion' vfilter));
            ]
      | ExistsEditor pfilter ->
        let%lwt editors = Model.Book.authors' book in
        Formula.interpret_exists (accepts_person pfilter) editors

  and accepts_dance filter dance =
    Formula.interpret filter @@ function
      | Core.Dance.Is dance' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id dance) dance'
      | Name string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.proximity ~char_equal string % NEString.to_string) @@ NEList.to_list @@ Model.Dance.names' dance
      | NameMatches string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.inclusion_proximity ~char_equal ~needle: string % NEString.to_string) @@ NEList.to_list @@ Model.Dance.names' dance
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Dance.kind' dance
      | ExistsDeviser pfilter ->
        let%lwt devisers = Model.Dance.devisers' dance in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) devisers in
        lwt (Formula.interpret_or_l scores)

  and accepts_person filter person =
    Formula.interpret filter @@ function
      | Core.Person.Is person' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.unsafe_equal (Entry.id person) person'
      | Name string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.Person.name' person
      | NameMatches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Person.name' person

  and accepts_set filter set =
    Formula.interpret filter @@ function
      | Core.Set.Is set' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id set) set'
      | Name string ->
        lwt @@ String.proximity ~char_equal string @@ NEString.to_string @@ Model.Set.name' set
      | NameMatches string ->
        lwt @@ String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Set.name' set
      | ExistsConceptor pfilter ->
        let%lwt conceptors = Model.Set.conceptors' set in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) conceptors in
        lwt (Formula.interpret_or_l scores)
      | ExistsVersion vfilter ->
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
      | NameMatches string ->
        lwt @@
          Formula.interpret_or
            (String.inclusion_proximity ~char_equal ~needle: string @@ NEString.to_string @@ Model.Source.name' source)
            (Option.fold ~none: Formula.interpret_false ~some: (String.inclusion_proximity ~char_equal ~needle: string % NEString.to_string) (Model.Source.short_name' source))
      | ExistsEditor pfilter ->
        let%lwt editors = Model.Source.editors' source in
        Formula.interpret_exists (accepts_person pfilter) editors

  and accepts_tune filter tune =
    Formula.interpret filter @@ function
      | Core.Tune.Is tune' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id tune) tune'
      | Name string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.proximity ~char_equal string % NEString.to_string) @@ NEList.to_list @@ Model.Tune.names' tune
      | NameMatches string ->
        lwt @@ Formula.interpret_or_l @@ List.map (String.inclusion_proximity ~char_equal ~needle: string % NEString.to_string) @@ NEList.to_list @@ Model.Tune.names' tune
      | ExistsComposer pfilter ->
        let%lwt composers = Model.Tune.composers' tune in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) composers in
        lwt (Formula.interpret_or_l scores)
      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ Model.Tune.kind' tune
      | ExistsDance dfilter ->
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
      | ExistsSource sfilter ->
        let%lwt sources = List.map (fun {Model.Version.source; _} -> source) <$> Model.Version.sources' version in
        Formula.interpret_exists (accepts_source sfilter) sources

  let rec accepts_any filter any =
    Formula.interpret filter @@ function
      | Core.Any.Raw string ->
        let lift_raw lift from_text_formula str =
          lift (Result.get_ok (from_text_formula (TextFormula.raw' str)))
        in
        flip accepts_any any @@
          Formula.or_l
            [
              lift_raw Core.Any.source' Core.Source.from_text_formula string;
              lift_raw Core.Any.person' Core.Person.from_text_formula string;
              lift_raw Core.Any.dance' Core.Dance.from_text_formula string;
              lift_raw Core.Any.book' Core.Book.from_text_formula string;
              lift_raw Core.Any.set' Core.Set.from_text_formula string;
              lift_raw Core.Any.tune' Core.Tune.from_text_formula string;
              lift_raw Core.Any.version' Core.Version.from_text_formula string;
            ]
      | Type type_ ->
        Model.Any.Type.equal (Model.Any.type_of any) type_
        |> Formula.interpret_bool
        |> lwt
      | Source sfilter ->
        (
          match any with
          | Source source -> accepts_source sfilter source
          | _ -> lwt Formula.interpret_false
        )
      | Person pfilter ->
        (
          match any with
          | Person person -> accepts_person pfilter person
          | _ -> lwt Formula.interpret_false
        )
      | Dance dfilter ->
        (
          match any with
          | Dance dance -> accepts_dance dfilter dance
          | _ -> lwt Formula.interpret_false
        )
      | Book bfilter ->
        (
          match any with
          | Book book -> accepts_book bfilter book
          | _ -> lwt Formula.interpret_false
        )
      | Set sfilter ->
        (
          match any with
          | Set set -> accepts_set sfilter set
          | _ -> lwt Formula.interpret_false
        )
      | Tune tfilter ->
        (
          match any with
          | Tune tune -> accepts_tune tfilter tune
          | _ -> lwt Formula.interpret_false
        )
      | Version vfilter ->
        (
          match any with
          | Version version -> accepts_version vfilter version
          | _ -> lwt Formula.interpret_false
        )
end
