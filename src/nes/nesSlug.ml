open Nes

type t = string

let of_string str =
  let charmap =
    Slug.Charmap.mk_charmap [
      Slug.Slug_data.base;
      [("-", " ")];
      (* ^ without this, dashes disappear, making [slugify] non-idempotent *)
    ]
  in
  Slug.slugify ~sep: "-" ~charmap ~lowercase: true str

let to_string = Fun.id

let add_suffix = (^)
