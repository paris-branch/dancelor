open Nes
open CreditLifted
include Dancelor_common_model.CreditFilter

let accepts filter credit =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Slug credit' ->
    let%lwt credit = Dancelor_common_model.CreditCore.slug credit in
    Lwt.return @@ Formula.interpret_bool @@ Slug.equal credit credit'

  | Line string ->
    let%lwt line = CreditLifted.line credit in
    Lwt.return (String.proximity ~char_equal string line)

  | LineMatches string ->
    let%lwt line = CreditLifted.line credit in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string line)

  | ExistsPerson pfilter ->
    persons credit
    >>=| Formula.interpret_exists (PersonFilter.accepts pfilter)
