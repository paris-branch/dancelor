open Nes
include Dancelor_common_model.DanceFilter

let accepts filter dance =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter @@ function

  | Is dance' ->
    DanceCore.equal dance dance' >|=| Formula.interpret_bool

  | Name string ->
    let%lwt name = DanceCore.name dance in
    Lwt.return (String.proximity ~char_equal string name)

  | NameMatches string ->
    let%lwt name = DanceCore.name dance in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle:string name)

  | Kind kfilter ->
    let%lwt kind = DanceCore.kind dance in
    KindFilter.Dance.accepts kfilter kind

  | Deviser cfilter ->
    (match%lwt DanceCore.deviser dance with
     | None -> Lwt.return Formula.interpret_false
     | Some deviser -> CreditFilter.accepts cfilter deviser)
