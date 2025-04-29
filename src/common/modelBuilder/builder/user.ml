open Nes

module Build
  (Person : Signature.Person.S)
= struct
  include Core.User

  let make ~person ?password ?password_reset_token ?remember_me_token () =
    make ~person: (Entry.slug person) ?password ?password_reset_token ?remember_me_token ()

  let update ?person ?password ?password_reset_token ?remember_me_token user =
    update ?person: (Option.map Entry.slug person) ?password ?password_reset_token ?remember_me_token user

  let person = Person.get % person
end
