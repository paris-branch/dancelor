open Nes
include Dancelor_common_model.PersonFilter

let accepts filter person =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Slug person' ->
    let%lwt person = PersonLifted.slug person in
    Lwt.return @@ Formula.interpret_bool @@ Slug.equal person person'

  | Name string ->
    let%lwt name = PersonLifted.name person in
    Lwt.return (String.proximity ~char_equal string name)

  | NameMatches string ->
    let%lwt name = PersonLifted.name person in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)
