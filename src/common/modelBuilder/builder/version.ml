open Nes

module Build (Getters : Getters.S) = struct
  include Core.Version

  let get = Getters.get_version

  let tune = Lwt.map Option.get % Getters.get_tune % tune
  let tune' = tune % Entry.value

  let sources = Lwt_list.map_p (Lwt.map Option.get % Getters.get_source) % sources
  let sources' = sources % Entry.value

  let arrangers = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % arrangers
  let arrangers' = arrangers % Entry.value

  let kind version =
    flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Core.Tune.kind' tune)
  let kind' = kind % Entry.value

  let names version = Core.Tune.names' <$> tune version
  let names' = names % Entry.value

  let one_name version = Core.Tune.one_name' <$> tune version
  let one_name' = one_name % Entry.value

  let other_names version = Core.Tune.other_names' <$> tune version
  let other_names' = other_names % Entry.value

  let slug version = Entry.Slug.of_string <$> one_name version
  let slug' = slug % Entry.value
end
