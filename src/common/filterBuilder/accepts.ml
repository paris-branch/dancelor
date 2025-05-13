open Nes

module Make (Model : ModelBuilder.S) = struct
  let char_equal = Char.Sensible.equal

  let rec accepts_book filter book =
    Formula.interpret filter @@ function
      | Core.Book.Is book' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug book) book'
      | Title string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Book.title' book
      | TitleMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Book.title' book
      | Subtitle string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Book.subtitle' book
      | SubtitleMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Book.subtitle' book
      | IsSource ->
        Lwt.return @@ Formula.interpret_bool @@ Model.Book.source' book
      | ExistsVersion vfilter ->
        let%lwt content = Model.Book.contents' book in
        let%lwt versions =
          Lwt_list.filter_map_s
            (* FIXME: unVersion *)
            (function
              | Model.Book.Version (v, _p) -> Lwt.return_some v
              | _ -> Lwt.return_none
            )
            content
        in
        Formula.interpret_exists (accepts_version vfilter) versions
      | ExistsSet sfilter ->
        let%lwt content = Model.Book.contents' book in
        let%lwt sets =
          Lwt_list.filter_map_s
            (* FIXME: unSet *)
            (function
              | Model.Book.Set (s, _p) -> Lwt.return_some s
              | _ -> Lwt.return_none
            )
            content
        in
        Formula.interpret_exists (accepts_set sfilter) sets
      | ExistsInlineSet sfilter ->
        let%lwt content = Model.Book.contents' book in
        let%lwt isets =
          Lwt_list.filter_map_s
            (* FIXME: unInlineSet *)
            (function
              | Model.Book.InlineSet (s, _p) -> Lwt.return_some s
              | _ -> Lwt.return_none
            )
            content
        in
        Formula.interpret_exists (accepts_set sfilter % Entry.make_dummy) isets
      | ExistsVersionDeep vfilter ->
        (* recursive call to check the compound formula *)
        Fun.flip accepts_book book @@
          Formula.or_l
            Core.[Formula.pred (Book.ExistsVersion vfilter);
            Formula.pred (Book.ExistsSet (Set.existsVersion' vfilter));
            Formula.pred (Book.ExistsInlineSet (Set.existsVersion' vfilter));
            ]

  and accepts_dance filter dance =
    Formula.interpret filter @@ function
      | Core.Dance.Is dance' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug dance) dance'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Dance.name' dance
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Dance.name' dance
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Dance.kind' dance
      | ExistsDeviser pfilter ->
        let%lwt devisers = Model.Dance.devisers' dance in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) devisers in
        Lwt.return (Formula.interpret_or_l scores)

  and accepts_person filter person =
    Formula.interpret filter @@ function
      | Core.Person.Is person' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.unsafe_equal (Entry.slug person) person'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Person.name' person
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Person.name' person

  and accepts_set filter set =
    Formula.interpret filter @@ function
      | Core.Set.Is set' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug set) set'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Set.name' set
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Set.name' set
      | ExistsConceptor pfilter ->
        let%lwt conceptors = Model.Set.conceptors' set in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) conceptors in
        Lwt.return (Formula.interpret_or_l scores)
      | ExistsVersion vfilter ->
        let%lwt contents = Model.Set.contents' set in
        Formula.interpret_exists (accepts_version vfilter % fst) contents
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Set.kind' set

  and accepts_source filter source =
    Formula.interpret filter @@ function
      | Core.Source.Is source' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.unsafe_equal (Entry.slug source) source'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Source.name' source
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Source.name' source

  and accepts_tune filter tune =
    Formula.interpret filter @@ function
      | Core.Tune.Is tune' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug tune) tune'
      | Name string ->
        Lwt.return @@ String.proximity ~char_equal string @@ Model.Tune.name' tune
      | NameMatches string ->
        Lwt.return @@ String.inclusion_proximity ~char_equal ~needle: string @@ Model.Tune.name' tune
      | ExistsComposer pfilter ->
        let%lwt composers = Model.Tune.composers' tune in
        let%lwt scores = Lwt_list.map_s (accepts_person pfilter) composers in
        Lwt.return (Formula.interpret_or_l scores)
      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ Model.Tune.kind' tune
      | ExistsDance dfilter ->
        let%lwt dances = Model.Tune.dances' tune in
        let%lwt scores = Lwt_list.map_s (accepts_dance dfilter) dances in
        Lwt.return (Formula.interpret_or_l scores)

  and accepts_version filter version =
    Formula.interpret filter @@ function
      | Core.Version.Is version' ->
        Lwt.return @@ Formula.interpret_bool @@ Slug.equal' (Entry.slug version) version'
      | Tune tfilter ->
        let%lwt tune = Model.Version.tune' version in
        accepts_tune tfilter tune
      | Key key' ->
        Lwt.return @@ Formula.interpret_bool (Model.Version.key' version = key')
      | Kind kfilter ->
        let%lwt tune = Model.Version.tune' version in
        Kind.Version.Filter.accepts kfilter (Model.Version.bars' version, Model.Tune.kind' tune)
      | ExistsSource sfilter ->
        let%lwt sources = Model.Version.sources' version in
        Formula.interpret_exists (accepts_source sfilter) sources

  let rec accepts_any filter any =
    Formula.interpret filter @@ function
      | Core.Any.Raw string ->
        let lift_raw lift from_text_formula str =
          lift (Result.get_ok (from_text_formula (TextFormula.raw' str)))
        in
        Fun.flip accepts_any any @@
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
        |> Lwt.return
      | Source sfilter ->
        (
          match any with
          | Source source -> accepts_source sfilter source
          | _ -> Lwt.return Formula.interpret_false
        )
      | Person pfilter ->
        (
          match any with
          | Person person -> accepts_person pfilter person
          | _ -> Lwt.return Formula.interpret_false
        )
      | Dance dfilter ->
        (
          match any with
          | Dance dance -> accepts_dance dfilter dance
          | _ -> Lwt.return Formula.interpret_false
        )
      | Book bfilter ->
        (
          match any with
          | Book book -> accepts_book bfilter book
          | _ -> Lwt.return Formula.interpret_false
        )
      | Set sfilter ->
        (
          match any with
          | Set set -> accepts_set sfilter set
          | _ -> Lwt.return Formula.interpret_false
        )
      | Tune tfilter ->
        (
          match any with
          | Tune tune -> accepts_tune tfilter tune
          | _ -> Lwt.return Formula.interpret_false
        )
      | Version vfilter ->
        (
          match any with
          | Version version -> accepts_version vfilter version
          | _ -> Lwt.return Formula.interpret_false
        )
end
