module Build (Getters : Getters.S) = struct
  include Core.Person

  let get = Getters.get_person
end
