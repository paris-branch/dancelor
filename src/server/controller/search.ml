open Nes

module type Searchable = sig
  type value
  type filter

  val get_all : Environment.t -> value Lwt_stream.t

  val filter_accepts : filter -> value -> float Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module type S = sig
  type value
  type filter

  val search : Environment.t -> Slice.t -> filter -> (int * value list) Lwt.t

  (* Pass through for better composition. FIXME: We should expose the sorted
     list with scores instead, because that can be assembled in the controller
     for Any as sorted streams and that is so much more efficient. *)
  val get_all : Environment.t -> value Lwt_stream.t
  val tiebreakers : (value -> value -> int Lwt.t) list
end

module Build (M : Searchable) : S with type value = M.value and type filter = M.filter = struct
  type value = M.value
  type filter = M.filter

  (* Hardcoded threshold for all of Dancelor. *)
  let threshold = 0.4

  let cache = Cache.create ~lifetime: 600 ()

  let search env slice filter =
    (* We cache the computation of the results but not the computation of the
       slice because that really isn't the expensive part. *)
    let%lwt results =
      Cache.use ~cache ~key: (env, threshold, filter) @@ fun () ->
      let values = M.get_all env in
      (* For each value, compute its score and return the pair (value, score). *)
      let values = Lwt_stream.map_s (fun value -> Pair.cons value <$> M.filter_accepts filter value) values in
      (* Keep only values whose score is above the given threshold. *)
      let values = Lwt_stream.filter (fun value -> snd value >= threshold) values in
      (* Sort by score, decreasing, falling back on the tiebreakers otherwise. *)
      let compare = Lwt_list.compare_multiple (Lwt_list.decreasing (lwt % snd) Float.compare :: List.map (fun compare -> fun x y -> compare (fst x) (fst y)) M.tiebreakers) in
      let%lwt values = Monadise_lwt.monadise_2_1 List.sort compare =<< Lwt_stream.to_list values in
      (* Remove the scores again. *)
      lwt @@ List.map fst values
    in
    (* Return the pair of the total number of results and the requested slice. *)
    lwt (List.length results, Slice.list ~strict: false slice results)

  (* Pass through for better composition *)
  let get_all = M.get_all
  let tiebreakers = M.tiebreakers
end
