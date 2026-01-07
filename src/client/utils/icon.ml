type t =
(* alerts *)
| Info_circle (** for info alerts *)
| Exclamation_triangle (** for warning alerts *)
| Exclamation_diamond (** for danger alerts *)
(* others *)
| Cloud_upload (** to show upload *)
| Hourglass_bottom (** to show wait *)
| Cpu (** to show computation *)

let to_string = function
  | Info_circle -> "info-circle"
  | Exclamation_triangle -> "exclamation-triangle"
  | Exclamation_diamond -> "exclamation-diamond"
  (* others *)
  | Cloud_upload -> "cloud-upload"
  | Hourglass_bottom -> "hourglass-bottom"
  | Cpu -> "cpu"

let html icon =
  let open Html in
  i ~a: [a_class ["bi"; ("bi-" ^ to_string icon)]] [];
