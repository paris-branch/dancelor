(** {1 Job id} *)

type t [@@deriving yojson]
(** Abstract type of a job id. *)

val to_string : t -> string
(** Convert a job id to a string. The string is guaranteed to contain only
    numbers and lowercase letters. *)

val of_string : string -> t option
(** Convert a string to a job id, if it has the right shape, or return
    [None]. *)

val create : unit -> t
(** Create a new random job id. There are about [10 ^ 31] ids, so the
    probability of a clash is close to none. *)
