module Build (Getters : Getters.S) = struct
  include Core.Source

  let get = Getters.get_source
end
