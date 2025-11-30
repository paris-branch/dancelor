open Nes

module Build (Getters : Getters.S) = struct
  include Core.Version

  let get = Getters.get_version

  let tune = Lwt.map Option.get % Getters.get_tune % tune
  let tune' = tune % Entry.value

  let source_core_to_source : source_core -> source Lwt.t = fun {source; structure; details} ->
    let%lwt source = Option.get <$> Getters.get_source source in
    lwt {source; structure; details}

  let sources = Lwt_list.map_p source_core_to_source % sources
  let sources' = sources % Entry.value

  let sources_grouped = Lwt_list.map_p (Lwt_list.map_p source_core_to_source) % sources_grouped
  let sources_grouped' = sources_grouped % Entry.value

  let arrangers = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % arrangers
  let arrangers' = arrangers % Entry.value

  let names version = Core.Tune.names' <$> tune version
  let names' = names % Entry.value

  let one_name version = Core.Tune.one_name' <$> tune version
  let one_name' = one_name % Entry.value

  let other_names version = Core.Tune.other_names' <$> tune version
  let other_names' = other_names % Entry.value

  let kind version = Core.Tune.kind' <$> tune version
  let kind' = kind % Entry.value

  let slug version = Entry.Slug.of_string % NEString.to_string <$> one_name version
  let slug' = slug % Entry.value

  let content_lilypond ?structure ?content: the_content version =
    let%lwt kind = kind version in
    let key = key version in
    let content = Option.value the_content ~default: (content version) in
    lwt @@ Content.lilypond ?structure ~kind ~key content

  let content_lilypond' ?structure ?content version =
    content_lilypond ?structure ?content @@ Entry.value version
end
