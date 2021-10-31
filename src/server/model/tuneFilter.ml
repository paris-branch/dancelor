open Nes
open TuneCore
include Dancelor_common_model.TuneFilter

let accepts filter tune =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Is tune' ->
    equal tune tune' >|=| Formula.interpret_bool

  | Name string ->
    let%lwt name = TuneCore.name tune in
    Lwt.return (String.proximity ~char_equal string name)

  | NameMatches string ->
    let%lwt name = TuneCore.name tune in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

  | Author afilter ->
    (match%lwt TuneCore.author tune with
     | None -> Formula.interpret_false |> Lwt.return
     | Some author -> CreditFilter.accepts afilter author)

  | Kind kfilter ->
    let%lwt kind = TuneCore.kind tune in
    KindFilter.Base.accepts kfilter kind
