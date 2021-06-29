open Nes

let _key = "person"

type t =
  { slug : t Slug.t ;
    status : Status.t [@default Status.bot] ;
    name : string }
[@@deriving yojson, make]

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let name p = Lwt.return p.name

let equal person1 person2 =
  let%lwt slug1 = slug person1 in
  let%lwt slug2 = slug person2 in
  Lwt.return (Slug.equal slug1 slug2)

module Filter = struct
  let _key = "person-filter"

  type person = t
  [@@deriving yojson]

  type t =
    | Is of person
    | HasName of string
  [@@deriving yojson]

  let accepts filter person =
    match filter with

    | Is person' ->
      let%lwt slug' = slug person' in
      let%lwt slug  = slug person  in
      Lwt.return (Slug.equal slug slug')

    | HasName name' ->
      let%lwt name = name person in
      Lwt.return (name = name')
end
