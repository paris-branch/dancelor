open Nes

module Build (Getters : Getters.S) = struct
  include Core.Dance

  let get = Getters.get_dance

  let devisers = Lwt_list.map_p Getters.get_person % devisers
  let devisers' = devisers % Entry.value

  let equal dance1 dance2 = Slug.equal' (Entry.slug dance1) (Entry.slug dance2)
end
