(* {1 Broadcast channels}

   This module implements “broadcast” channels. They are a mean of handling
   communication between concurrent threads. They are similar to {!Lwt_mvar}
   except for two fundamental differences:

   1. {!Lwt_mvar} is limited to only one thread listening: if two threads
      listen, they will get one message each. Broadcast channels allow to
      define an arbitrary number of “ports” which can all listen at the same
      time on the same broadcast channel.

   2. {!Lwt_mvar} ensures synchronisation: putting a new value is blocking and
      waits for the previous value to be gone, ensuring that the listener got
      the value in question. Broadcast channels move on: if the listeners start
      listening after a change, they will not see the previous value.
*)

type 'a t
(** The type of a broadcast channel. Broadcast channels are used to communicate
    values between threads. The type parameter specifies the type of the value
    propagated from {!put} to {!take}. *)

type 'a port
(** The type of a broadcast port. Several broadcast ports can be tied to the
    same channel; all the ports will receive the updates. The type parameter
    specifies the type of the value propagated from {!put} to {!take}. *)

val create : 'a -> 'a t
(** [create v] creates a new channel containing value [v]. *)

val create_port : 'a t -> 'a port
(** [create_port chan] creates a new port for channel [chan]. *)

val put : 'a t -> 'a -> unit
(** [put chan v] updates the value of channel [chan] to [v] and notifies all the
    listening ports of the update. *)

val take : 'a port -> 'a Lwt.t
(** [take port] returns the last unread value of the channel associated to
    [port]. This may be blocking if there is no such value yet. Only one thread
    should call [take] on a given port. *)
