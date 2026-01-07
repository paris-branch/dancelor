(** Type for access-specific icons. *)
type access =
  | Everyone
  | Viewer
  | Owner
  | Omniscient_administrator

let access_to_string = function
  | Everyone -> "globe"
  | Viewer -> "eye"
  | Owner -> "unlock-fill" (* FIXME: unlock2-fill would be better but requires Bootstrap icons 1.13.1 *)
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

(** Type for model-specific icons. *)
type model =
  | Source
  | Person
  | Dance
  | Tune
  | Version
  | Set
  | Book
  | User

let model_to_string = function
  | Source -> "archive"
  | Person -> "person"
  | Dance -> "person-arms-up"
  | Tune -> "music-note-list"
  | Version -> "music-note-beamed"
  | Set -> "list-stars"
  | Book -> "book"
  | User -> "person-circle"

type t =
  | Access of access
  | Alert of alert
  | Job of job
  | Model of model

let to_string = function
  | Access icon -> access_to_string icon
  | Alert icon -> alert_to_string icon
  | Job icon -> job_to_string icon
  | Model icon -> model_to_string icon

(** Generate HTML for the given icon. One can optionally pass extra HTML
    [?classes] or a [?tooltip] that is shown when hovering on the icon. *)
let html ?(classes = []) ?tooltip icon =
  let open Html in
  let a =
    List.concat [
      [a_class (["bi"; ("bi-" ^ to_string icon)] @ classes)];
      (Option.fold ~none: [] ~some: (fun t -> [a_title t]) tooltip);
    ]
  in
  i ~a []
