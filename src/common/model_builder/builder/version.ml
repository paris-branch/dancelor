open Nes

module Build (Getters : Getters.S) = struct
  include Core.Version

  let get = Getters.get_version

  let tune = Lwt.map Option.get % Getters.get_tune % tune
  let tune' = tune % Entry.value_public

  let names version = Core.Tune.names' <$> tune version
  let names' = names % Entry.value_public

  let one_name version = Core.Tune.one_name' <$> tune version
  let one_name' = one_name % Entry.value_public

  let other_names version = Core.Tune.other_names' <$> tune version
  let other_names' = other_names % Entry.value_public

  let kind version = Core.Tune.kind' <$> tune version
  let kind' = kind % Entry.value_public

  let slug version = NesSlug.of_string % NEString.to_string <$> one_name version
  let slug' = slug % Entry.value_public

  let content_lilypond ?structure ?content: the_content version =
    let%lwt kind = kind version in
    let key = key version in
    let content = Option.value the_content ~default: (content version) in
    lwt @@ Content.lilypond ?structure ~kind ~key content

  let content_lilypond' ?structure ?content version =
    content_lilypond ?structure ?content @@ Entry.value_public version
end
