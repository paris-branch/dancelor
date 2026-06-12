include module type of Lwt_stream

type 'a next = Next of 'a | Last of 'a

val from_next : (unit -> 'a next Lwt.t) -> 'a t
(** Variant of {!from} for when the function can return a “last” element. The
    stream is terminated after it. *)

val get_available_1 : 'a t -> 'a option
(** Variant of {!get_available} that returns only the first element. Note that
    [None], in this case, represents the absence of elements, not the end of the
    stream. *)

val return_lwt : 'a Lwt.t -> 'a t [@@alert unsafe "Lwt_stream.return_lwt has the bad practice of ignoring its argument; use NesLwt_stream.return_lwt' instead"]

val return_lwt' : 'a Lwt.t -> 'a t
(** Variant of {!Lwt_stream.return_lwt} which, when the promise becomes
    rejected, forwards the exception to the consumer of the stream, instead of
    simply returning an empty stream. *)

val flip_lwt : 'a t Lwt.t -> 'a t
