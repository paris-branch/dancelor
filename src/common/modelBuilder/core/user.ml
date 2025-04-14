open Nes

let _key = "user"

type t = {
  name: string;
  person: Person.t Slug.t;
  password: HashedPassword.t;
}
[@@deriving eq, yojson, make, show {with_path = false}, fields]

let name = name % Entry.value
let person = person % Entry.value
let password = password % Entry.value
