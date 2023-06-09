open Nes
open VersionLifted
include Dancelor_common_model.VersionFilter

let accepts filter version =
  Formula.interpret filter @@ function

  | Is version' ->
    equal version version' >|=| Formula.interpret_bool

  | Tune tfilter ->
    let%lwt tune = VersionLifted.tune version in
    TuneFilter.accepts tfilter tune

  | Key key' ->
    let%lwt key = VersionLifted.key version in
    Lwt.return (Formula.interpret_bool (key = key'))

  | Kind kfilter ->
    let%lwt tune = VersionLifted.tune version in
    let%lwt kind = Tune.kind tune in
    let%lwt bars = bars version in
    KindFilter.Version.accepts kfilter (bars, kind)

  | Broken ->
    VersionLifted.broken version >|=| Formula.interpret_bool
