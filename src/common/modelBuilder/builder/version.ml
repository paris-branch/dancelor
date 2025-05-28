open Nes

module Build (Getters : Getters.S) = struct
  include Core.Version

  let get = Getters.get_version

  let tune = Getters.get_tune % tune
  let tune' = tune % Entry.value

  let sources = Lwt_list.map_p Getters.get_source % sources
  let sources' = sources % Entry.value

  let arrangers = Lwt_list.map_p Getters.get_person % arrangers
  let arrangers' = arrangers % Entry.value

  let kind version =
    flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Core.Tune.kind' tune)
  let kind' = kind % Entry.value

  let name version = Core.Tune.name' <$> tune version
  let name' = name % Entry.value
end
