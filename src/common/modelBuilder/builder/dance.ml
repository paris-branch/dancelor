open Nes

module Build (Getters : Getters.S) = struct
  include Core.Dance

  let get = Getters.get_dance

  let make ~name ~kind ?devisers ?two_chords ?scddb_id ?disambiguation ?date () =
    let name = String.remove_duplicates ~char: ' ' name in
    let disambiguation = Option.map (String.remove_duplicates ~char: ' ') disambiguation in
    let devisers = Option.map (List.map Entry.slug) devisers in
    make ~name ~kind ?devisers ~two_chords ~scddb_id ?disambiguation ~date ()

  let devisers = Lwt_list.map_p Getters.get_person % devisers
  let devisers' = devisers % Entry.value

  let equal dance1 dance2 = Slug.equal' (Entry.slug dance1) (Entry.slug dance2)
end
