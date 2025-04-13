open Nes

module Build
  (Person : Signature.Person.S)
= struct
  include Core.User

  let make ~name ~person () =
    let person = Entry.slug person in
    make ~name ~person

  let person = Person.get % person
end
