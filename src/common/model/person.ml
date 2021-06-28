open Nes

module Self = struct
  type t =
    { slug : t Slug.t ;
      status : Status.t [@default Status.bot] ;
      name : string }
  [@@deriving yojson, make]

  let _key = "person"
end
include Self

let slug p = Lwt.return p.slug
let status p = Lwt.return p.status
let name p = Lwt.return p.name

module Filter = struct
  type t =
    | Is of Self.t
    | HasName of string
  [@@deriving yojson]

  let _key = "person-filter"

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
