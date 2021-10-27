open Nes
open CreditCore
include Dancelor_common_model.CreditFilter

let accepts filter credit =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Is credit' ->
    equal credit credit' >|=| Formula.interpret_bool

  | Line string ->
    let%lwt line = CreditCore.line credit in
    Lwt.return (String.proximity ~char_equal string line)

  | LineMatches string ->
    let%lwt line = CreditCore.line credit in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string line)

  | ExistsPerson pfilter ->
    CreditCore.persons credit
    >>=| Formula.interpret_exists (PersonFilter.accepts pfilter)
