module Build (Getters : Getters.S) = struct
  include Core.Tune

  let get = Getters.get_tune
end
