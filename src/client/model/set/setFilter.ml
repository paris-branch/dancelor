open Nes
include Dancelor_common_model.SetFilter

let accepts filter set =
  let char_equal = Char.Sensible.equal in
  Formula.interpret filter
  @@ function
  | Is set' ->
    Set.equal set set' >|=| Formula.interpret_bool
  | Name string ->
    let%lwt name = Set.name set in
    Lwt.return (String.proximity ~char_equal string name)
  | NameMatches string ->
    let%lwt name = Set.name set in
    Lwt.return (String.inclusion_proximity ~char_equal ~needle: string name)
  | Deviser dfilter ->
    (
      match%lwt Set.deviser set with
      | None -> Lwt.return Formula.interpret_false
      | Some deviser -> CreditFilter.accepts dfilter deviser
    )
  | ExistsVersion vfilter ->
    let%lwt versions_and_parameters = Set.versions_and_parameters set in
    Formula.interpret_exists
      (
        fun (version, _) ->
          VersionFilter.accepts vfilter version
      )
      versions_and_parameters
  | Kind kfilter ->
    let%lwt kind = Set.kind set in
    KindFilter.Dance.accepts kfilter kind
