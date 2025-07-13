open Nes

module Build (Getters : Getters.S) = struct
  include Core.Source

  let get = Getters.get_source

  let editors = Lwt_list.map_p Getters.get_person % editors
  let editors' = editors % Entry.value
end
