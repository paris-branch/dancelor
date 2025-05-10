open Nes

module Build
  (Person : Signature.Person.S)
= struct
  include Core.User

  let make ~display_name ~person ?password ?password_reset_token ?remember_me_tokens () =
    make ~display_name ~person: (Entry.slug person) ?password ?password_reset_token ?remember_me_tokens ()

  let update ?display_name ?person ?password ?password_reset_token ?remember_me_tokens user =
    update
      ?display_name
      ?person: (Option.map (fun person slug -> Lwt.bind (Person.get slug) (Lwt.map Entry.slug % person)) person)
      ?password
      ?password_reset_token
      ?remember_me_tokens
      user

  let person = Person.get % person
  let person' = person % Entry.value
end
