type level = Info | Warning | Danger

let level_to_bootstrap_class = function
  | Info -> "alert-info"
  | Warning -> "alert-warning"
  | Danger -> "alert-danger"

let level_to_default_bootstrap_icon = function
  | Info -> Icon.InfoCircle
  | Warning -> Icon.ExclamationTriangle
  | Danger -> Icon.ExclamationDiamond

let make ~level ?icon content =
  let open Html in
  div ~a: [a_class ["m-0"; "alert"; level_to_bootstrap_class level; "d-flex"]] [
    Icon.html (Option.value icon ~default: (level_to_default_bootstrap_icon level));
    div ~a: [a_class ["ms-2"]] content;
  ];
