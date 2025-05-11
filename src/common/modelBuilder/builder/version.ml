open Nes

module Build (Getters : Getters.S) = struct
  include Core.Version

  let get = Getters.get_version

  let make ~tune ~bars ~key ~structure ?sources ?arrangers ?remark ?disambiguation ~content () =
    let structure = String.remove_duplicates ~char: ' ' structure in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let tune = Entry.slug tune in
    let sources = Option.map (List.map Entry.slug) sources in
    let arrangers = Option.map (List.map Entry.slug) arrangers in
    make ~tune ~bars ~key ~structure ?sources ?arrangers ?remark ?disambiguation ~content ()

  let tune = Getters.get_tune % tune
  let tune' = tune % Entry.value

  let sources = Lwt_list.map_p Getters.get_source % sources
  let sources' = sources % Entry.value

  let arrangers = Lwt_list.map_p Getters.get_person % arrangers
  let arrangers' = arrangers % Entry.value

  let kind version =
    Fun.flip Lwt.map (tune version) @@ fun tune ->
    (bars version, Core.Tune.kind' tune)
  let kind' = kind % Entry.value

  let name version = Lwt.map Core.Tune.name' (tune version)
  let name' = name % Entry.value
end
