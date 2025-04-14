open Nes

module Build
  (Person : Signature.Person.S)
= struct
  include Core.User

  let make ~name ~person ~password () =
    let person = Entry.slug person in
    make ~name ~person ~password

  let person = Person.get % person
end
