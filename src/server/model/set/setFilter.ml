open Nes
include Dancelor_common_model.SetFilter

let accepts filter set =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter
  @@ function
  | Is set' ->
    SetLifted.equal set set' >|=| Formula.interpret_bool
  | Name string ->
    let%lwt name = SetLifted.name set in
    Lwt.return (String.proximity ~char_equal string name)
  | NameMatches string ->
    let%lwt name = SetLifted.name set in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle: string name)
  | Deviser dfilter ->
    (
      match%lwt SetLifted.deviser set with
      | None -> Lwt.return Formula.interpret_false
      | Some deviser -> CreditFilter.accepts dfilter deviser
    )
  | ExistsVersion vfilter ->
    let%lwt versions_and_parameters = SetLifted.versions_and_parameters set in
    Formula.interpret_exists
      (
        fun (version, _) ->
          VersionFilter.accepts vfilter version
      )
      versions_and_parameters
  | Kind kfilter ->
    let%lwt kind = SetLifted.kind set in
    KindFilter.Dance.accepts kfilter kind
