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

val singleton : 'a -> 'a t
(** Create a non-empty list with a single element. *)
