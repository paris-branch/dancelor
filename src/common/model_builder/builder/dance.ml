module Build (Getters : Getters.S) = struct
  include Core.Dance

  let get = Getters.get_dance
end
