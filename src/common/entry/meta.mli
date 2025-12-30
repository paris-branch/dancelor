open Nes

type t
[@@deriving show, yojson]

val status : t -> Status.t
val privacy : t -> Privacy.t
val created_at : t -> Datetime.t
val modified_at : t -> Datetime.t

val make :
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  unit ->
  t

val update :
  ?status: Status.t ->
  ?privacy: Privacy.t ->
  ?created_at: Datetime.t ->
  ?modified_at: Datetime.t ->
  t ->
  t
