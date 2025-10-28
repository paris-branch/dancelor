(** {1 Non-empty list} *)

type 'a t [@@deriving eq, show, yojson]
(** A non-empty list. *)

val to_list : 'a t -> 'a list
(** Convert a non-empty list to a regular list. *)

val of_list : 'a list -> 'a t option
(** Convert a list to a non-empty list. *)

val of_list_exn : 'a list -> 'a t
(** Convert a list to a non-empty list, or raise {!Invalid_argument} if the list
    is empty. *)

val hd : 'a t -> 'a
(** Get the head of a non-empty list, that is the first element of the list. *)

val tl : 'a t -> 'a list
(** Get the tail of a non-empty list, that is the list without its first
    element. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map a function over a non-empty list. *)

val map_lwt_s : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t
(** Similar to {!Lwt_list.map_s} for non-empty lists. *)

val map_lwt_p : ('a -> 'b Lwt.t) -> 'a t -> 'b t Lwt.t
(** Similar to {!Lwt_list.map_p} for non-empty lists. *)

val singleton : 'a -> 'a t
(** Create a non-empty list with a single element. *)

val append : 'a t -> 'a t -> 'a t
(** Appending non-empty lists produces a non-empty list. *)

val fold_left1 : ('a -> 'a -> 'a) -> 'a t -> 'a
(** Like {!List.fold_left} but uses the first element as accumlator. *)

val is_singleton : 'a t -> bool
(** Check if the non-empty list contains a single element. *)
