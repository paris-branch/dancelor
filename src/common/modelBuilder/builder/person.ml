open Nes

module Build (Getters : Getters.S) = struct
  include Core.Person

  let get = Getters.get_person

  let user person =
    match user person with
    | None -> lwt_none
    | Some user -> Getters.get_user user

  let user' = user % Entry.value_public
end
