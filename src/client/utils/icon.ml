type t =
(* alerts *)
| InfoCircle (** for info alerts *)
| ExclamationTriangle (** for warning alerts *)
| ExclamationDiamond (** for danger alerts *)
(* others *)
| CloudUpload (** to show upload *)
| HourglassBottom (** to show wait *)
| Cpu (** to show computation *)

let to_string = function
  | InfoCircle -> "info-circle"
  | ExclamationTriangle -> "exclamation-triangle"
  | ExclamationDiamond -> "exclamation-diamond"
  (* others *)
  | CloudUpload -> "cloud-upload"
  | HourglassBottom -> "hourglass-bottom"
  | Cpu -> "cpu"

let html icon =
  let open Html in
  i ~a: [a_class ["bi"; ("bi-" ^ to_string icon)]] [];
