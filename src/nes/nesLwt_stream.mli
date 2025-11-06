include module type of Lwt_stream

type 'a next = Next of 'a | Last of 'a

val from_next : (unit -> 'a next Lwt.t) -> 'a t
(** Variant of {!from} for when the function can return a “last” element. The
    stream is terminated after it. *)

val get_available_1 : 'a t -> 'a option
(** Variant of {!get_available} that returns only the first element. Note that
    [None], in this case, represents the absence of elements, not the end of the
    stream. *)

val choose_biased : 'a t list -> 'a t
(** Variant of {!Lwt_stream.choose} that tries to pick in order. If one of the
    streams gets closed, then it raises {!Lwt_stream.Empty}. FIXME: we could
    continue with the other streams until they are all closed. *)
