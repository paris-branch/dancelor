open Nes

module Build (Getters : Getters.S) = struct
  include Core.Tune

  let get = Getters.get_tune

  let make ~name ?alternative_names ~kind ?composers ?dances ?remark ?scddb_id ?date () =
    let name = String.remove_duplicates ~char: ' ' name in
    let alternative_names = Option.map (List.map (String.remove_duplicates ~char: ' ')) alternative_names in
    let composers = Option.map (List.map Entry.slug) composers in
    let dances = Option.map (List.map Entry.slug) dances in
    make ~name ?alternative_names ~kind ?composers ?dances ?remark ~scddb_id ~date ()

  let composers = Lwt_list.map_p Getters.get_person % composers
  let composers' = composers % Entry.value

  let dances = Lwt_list.map_p Getters.get_dance % dances
  let dances' = dances % Entry.value
end
