open Nes

type t
[@@deriving show, yojson]

val created_at : t -> Datetime.t
val modified_at : t -> Datetime.t

val make :
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  unit ->
  t

val update :
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  t ->
  t
