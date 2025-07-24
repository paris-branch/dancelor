open Nes

module Build (Getters : Getters.S) = struct
  include Core.User

  let update ?username ?person ?password ?password_reset_token ?remember_me_tokens user =
    update
      ?username
      ?person: (Option.map (fun person id -> Lwt.map Entry.id % person =<< Getters.get_person id) person)
      ?password
      ?password_reset_token
      ?remember_me_tokens
      user

  let person = Getters.get_person % person
  let person' = person % Entry.value
end
