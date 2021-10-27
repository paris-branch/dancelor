open Nes
open VersionCore
include Dancelor_common_model.VersionFilter

let accepts filter version =
  Formula.interpret filter @@ function

  | Is version' ->
    equal version version' >|=| Formula.interpret_bool

  | Tune tfilter ->
    let%lwt tune = VersionCore.tune version in
    TuneFilter.accepts tfilter tune

  | Key key' ->
    let%lwt key = VersionCore.key version in
    Lwt.return (Formula.interpret_bool (key = key'))

  | BarsEq bars' ->
    let%lwt bars = VersionCore.bars version in
    Lwt.return (Formula.interpret_bool (bars = bars'))
  | BarsNe bars' ->
    let%lwt bars = VersionCore.bars version in
    Lwt.return (Formula.interpret_bool (bars <> bars'))
  | BarsGt bars' ->
    let%lwt bars = VersionCore.bars version in
    Lwt.return (Formula.interpret_bool (bars > bars'))
  | BarsGe bars' ->
    let%lwt bars = VersionCore.bars version in
    Lwt.return (Formula.interpret_bool (bars >= bars'))
  | BarsLt bars' ->
    let%lwt bars = VersionCore.bars version in
    Lwt.return (Formula.interpret_bool (bars < bars'))
  | BarsLe bars' ->
    let%lwt bars = VersionCore.bars version in
    Lwt.return (Formula.interpret_bool (bars <= bars'))
