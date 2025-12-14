open Nes

module type Searchable = sig
  type value
  type filter

  val get_all : Environment.t -> value Lwt_stream.t

  val optimise_filter : filter -> filter
  val filter_is_empty : filter -> bool

  val filter_accepts : filter -> value -> float Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module type S = sig
  type value
  type filter

  val search : Environment.t -> Slice.t -> filter -> (int * value list) Lwt.t

  val search' : Environment.t -> filter -> (int * (value * float) Seq.t) Lwt.t
  (** Variant of {!search} that exposes the whole sequence of values, sorted and
      with their scores, but before slicing. *)

  (* Pass through for better composition. *)
  val tiebreakers : (value -> value -> int Lwt.t) list
end

module Build (M : Searchable) : S with type value = M.value and type filter = M.filter = struct
  type value = M.value
  type filter = M.filter

  (* Hardcoded threshold for all of Dancelor. *)
  let threshold = 0.4

  let cache = Cache.create ~lifetime: 600 ()

  let search' env filter =
    let filter = M.optimise_filter filter in
    Cache.use ~cache ~key: (env, threshold, filter) @@ fun () ->
    if M.filter_is_empty filter then
      lwt (0, Seq.empty)
    else
      (
        let values = M.get_all env in
        (* For each value, compute its score and return the pair (value, score). *)
        let values = Lwt_stream.map_s (fun value -> Pair.cons value <$> M.filter_accepts filter value) values in
        (* Keep only values whose score is above the given threshold. *)
        let values = Lwt_stream.filter (fun value -> snd value >= threshold) values in
        (* Finally, sort by score, decreasing, falling back on the tiebreakers otherwise. *)
        let compare = Lwt_list.compare_multiple (Lwt_list.decreasing (lwt % snd) Float.compare :: List.map (fun compare -> fun x y -> compare (fst x) (fst y)) M.tiebreakers) in
        let%lwt values = Monadise_lwt.monadise_2_1 List.sort compare =<< Lwt_stream.to_list values in
        lwt (List.length values, List.to_seq values)
      )

  let search env slice filter =
    let%lwt (count, results) = search' env filter in
    lwt (count, List.of_seq @@ Slice.seq ~strict: false slice @@ Seq.map fst results)

  (* Pass through for better composition *)
  let tiebreakers = M.tiebreakers
end
