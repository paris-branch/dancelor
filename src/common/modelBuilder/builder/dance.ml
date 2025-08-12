open Nes

module Build (Getters : Getters.S) = struct
  include Core.Dance

  let get = Getters.get_dance

  let devisers = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % devisers
  let devisers' = devisers % Entry.value

  let equal dance1 dance2 = Entry.Id.equal' (Entry.id dance1) (Entry.id dance2)
end
