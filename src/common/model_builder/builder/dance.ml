open Nes

module Build (Getters : Getters.S) = struct
  include Core.Dance

  let get = Getters.get_dance

  let devisers = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % devisers
  let devisers' = devisers % Entry.value_public
end
