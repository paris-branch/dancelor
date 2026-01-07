(** Type for access-specific icons. *)
type access =
  | Everyone
  | Viewer
  | Owner
  | Omniscient_administrator

let access_to_string = function
  | Everyone -> "globe"
  | Viewer -> "unlock"
  | Owner -> "lock-fill"
  | Omniscient_administrator -> "shield-lock-fill"

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
  | Access of access
  | Alert of alert
  | Job of job

let to_string = function
  | Access icon -> access_to_string icon
  | Alert icon -> alert_to_string icon
  | Job icon -> job_to_string icon

(** Generate HTML for the given icon. Optionally, one can pass a tooltip that is
    shown when hovering on the icon. *)
let html ?tooltip icon =
  let open Html in
  let a =
    List.concat [
      [a_class ["bi"; ("bi-" ^ to_string icon)]];
      (Option.fold ~none: [] ~some: (fun t -> [a_title t]) tooltip);
    ]
  in
  i ~a []
