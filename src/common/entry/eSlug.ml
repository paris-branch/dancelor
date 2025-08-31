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

module S : Madge.STRINGABLE with type t = t = struct
  type nonrec t = t
  let of_string = Option.some % of_string
  let to_string = to_string
end
