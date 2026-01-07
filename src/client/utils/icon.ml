(** Type for alert-specific icons. *)
type alert =
  | Info
  | Warning
  | Danger

let alert_to_string = function
  | Info -> "info-circle"
  | Warning -> "exclamation-triangle"
  | Danger -> "exclamation-diamond"

(** Type for job-specific icons. *)
type job =
  | Registering (** aka uploading *)
  | Pending
  | Running

let job_to_string = function
  | Registering -> "cloud-upload"
  | Pending -> "hourglass-bottom"
  | Running -> "cpu"

type t =
  | Alert of alert
  | Job of job

let to_string = function
  | Alert icon -> alert_to_string icon
  | Job icon -> job_to_string icon

let html icon =
  let open Html in
  i ~a: [a_class ["bi"; ("bi-" ^ to_string icon)]] [];
