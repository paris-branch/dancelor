open Nes

module Build (Getters : Getters.S) = struct
  include Core.Source

  let get = Getters.get_source

  let editors = Lwt_list.map_p (Lwt.map Option.get % Getters.get_person) % editors
  let editors' = editors % Entry.value_public
end
