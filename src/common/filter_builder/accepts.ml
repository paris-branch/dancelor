open Nes

module Make (Model : Model_builder.S) = struct
  let char_equal = Char.Sensible.equal

  let rec accepts_user filter user =
    Formula.interpret filter @@ function
      | Core.User.Is user' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.unsafe_equal (Entry.id user) user'
      | Username sfilter ->
        Formula_string.accepts sfilter @@ Model.User.Username.to_string @@ Model.User.username' user

  and accepts_book filter book =
    Formula.interpret filter @@ function
      | Core.Book.Is book' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id book) book'
      | Title sfilter ->
        Formula_string.accepts sfilter @@ NEString.to_string @@ Model.Book.title' book
      | Versions vfilter ->
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
        Formula_list.accepts accepts_version vfilter versions
      | Sets sfilter ->
        let%lwt content = Model.Book.contents' book in
        let%lwt sets =
          Lwt_list.filter_map_s
            (function
              | Model.Book.Set (s, _) | Model.Book.Dance (_, Dance_set (s, _)) -> lwt_some s
              | _ -> lwt_none
            )
            content
        in
        Formula_list.accepts accepts_set sfilter sets
      | Versions_deep vfilter ->
        (* recursive call to check the compound formula *)
        flip accepts_book book @@
          Formula.or_l
            Core.[Book.versions' vfilter;
            Book.sets' (Formula_list.exists' (Set.versions' vfilter));
            ]
      | Editors pfilter ->
        let%lwt editors = Model.Book.authors' book in
        Formula_list.accepts accepts_person' pfilter editors
      | Owners lfilter ->
        let owners = NEList.to_list @@ Entry.(Access.Private.owners % access) book in
        let%lwt owners = Lwt_list.map_p (Lwt.map Option.get % Model.User.get) owners in
        Formula_list.accepts accepts_user lfilter owners

  and accepts_dance filter dance =
    Formula.interpret filter @@ function
      | Core.Dance.Is dance' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id dance) dance'
      | Name sfilter ->
        Formula.interpret_or_l
        <$> Lwt_list.map_s (Formula_string.accepts sfilter % NEString.to_string) @@ NEList.to_list @@ Model.Dance.names' dance
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Dance.kind' dance
      | Devisers pfilter ->
        let%lwt devisers = Model.Dance.devisers' dance in
        Formula_list.accepts accepts_person' pfilter devisers

  and accepts_person filter person =
    Formula.interpret filter @@ function
      | Core.Person.Name sfilter ->
        Formula_string.accepts sfilter @@ NEString.to_string @@ Model.Person.name person

  and accepts_person' filter entry =
    Formula_entry.accepts accepts_person filter entry

  and accepts_set filter set =
    Formula.interpret filter @@ function
      | Core.Set.Is set' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id set) set'
      | Name sfilter ->
        Formula_string.accepts sfilter @@ NEString.to_string @@ Model.Set.name' set
      | Conceptors pfilter ->
        let%lwt conceptors = Model.Set.conceptors' set in
        Formula_list.accepts accepts_person' pfilter conceptors
      | Versions vfilter ->
        let%lwt versions = List.map fst <$> Model.Set.contents' set in
        Formula_list.accepts accepts_version vfilter versions
      | Kind kfilter ->
        Kind.Dance.Filter.accepts kfilter @@ Model.Set.kind' set
      | Owners lfilter ->
        let owners = NEList.to_list @@ Entry.(Access.Private.owners % access) set in
        let%lwt owners = Lwt_list.map_p (Lwt.map Option.get % Model.User.get) owners in
        Formula_list.accepts accepts_user lfilter owners

  and accepts_source filter source =
    Formula.interpret filter @@ function
      | Core.Source.Is source' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.unsafe_equal (Entry.id source) source'
      | Name sfilter ->
        Lwt.l2
          Formula.interpret_or
          (Formula_string.accepts sfilter @@ NEString.to_string @@ Model.Source.name' source)
          (Option.fold ~none: (lwt Formula.interpret_false) ~some: (Formula_string.accepts sfilter % NEString.to_string) @@ Model.Source.short_name' source)
      | Editors pfilter ->
        let%lwt editors = Model.Source.editors' source in
        Formula_list.accepts accepts_person' pfilter editors

  and accepts_tune filter tune =
    Formula.interpret filter @@ function
      | Core.Tune.Is tune' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id tune) tune'
      | Name sfilter ->
        Formula.interpret_or_l <$> Lwt_list.map_s (Formula_string.accepts sfilter % NEString.to_string) @@ NEList.to_list @@ Model.Tune.names' tune
      | Composers pfilter ->
        let%lwt composers = List.map Model.Tune.composer_composer <$> Model.Tune.composers' tune in
        Formula_list.accepts accepts_person' pfilter composers
      | Kind kfilter ->
        Kind.Base.Filter.accepts kfilter @@ Model.Tune.kind' tune
      | Dances dfilter ->
        let%lwt dances = Model.Tune.dances' tune in
        Formula_list.accepts accepts_dance dfilter dances

  and accepts_version filter version =
    Formula.interpret filter @@ function
      | Core.Version.Is version' ->
        lwt @@ Formula.interpret_bool @@ Entry.Id.equal' (Entry.id version) version'
      | Tune tfilter ->
        let%lwt tune = Model.Version.tune' version in
        accepts_tune tfilter tune
      | Key key' ->
        lwt @@ Formula.interpret_bool (Model.Version.key' version = key')
      | Sources sfilter ->
        let%lwt sources = List.map (fun {Model.Version.source; _} -> source) <$> Model.Version.sources' version in
        Formula_list.accepts accepts_source sfilter sources
end
