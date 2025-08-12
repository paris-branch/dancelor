open Nes

module Build (Getters : Getters.S) = struct
  include Core.Tune

  let get = Getters.get_tune

  let composers = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % composers
  let composers' = composers % Entry.value

  let dances = Lwt_list.map_p (Lwt.map Option.get % Getters.get_dance) % dances
  let dances' = dances % Entry.value
end
