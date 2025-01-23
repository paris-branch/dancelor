open Nes

module type Searchable = sig
  type value
  type filter

  val cache : (float * filter, value list Lwt.t) Cache.t

  val get_all : unit -> value list Lwt.t

  val filter_accepts : filter -> value -> float Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module type S = sig
  type value
  type filter

  val search : Slice.t -> filter -> (int * value list) Lwt.t

  val search' : filter -> value list Lwt.t
  val count : filter -> int Lwt.t

  val tiebreakers : (value -> value -> int Lwt.t) list
end

module Make (M : Searchable) : S with type value = M.value and type filter = M.filter = struct
  type value = M.value
  type filter = M.filter

  (* Hardcoded threshold for all of Dancelor. *)
  let threshold = 0.4

  let search slice filter =
    (* We cache the computation of the results but not the computation of the
       slice because that really isn't the expensive part. *)
    let%lwt results =
      Cache.use ~cache: M.cache ~key: (threshold, filter) @@ fun () ->
      let%lwt values = M.get_all () in
      (* For each value, compute its score and return the pair (value, score). *)
      let%lwt values = Lwt_list.map_s (fun value -> Lwt.map (pair value) (M.filter_accepts filter value)) values in
      (* Keep only values whose score is above the given threshold. *)
      let values = List.filter (fun value -> snd value >= threshold) values in
      (* Sort by score, decreasing, falling back on the tiebreakers otherwise. *)
      let comparisons =
        Lwt_list.decreasing (Lwt.return % snd) Float.compare :: List.map (fun compare -> fun x y -> compare (fst x) (fst y)) M.tiebreakers
      in
      let%lwt values = Lwt_list.sort_multiple comparisons values in
      (* Remove the scores again. *)
      Lwt.return @@ List.map fst values
    in
    (* Return the pair of the total number of results and the requested slice. *)
    Lwt.return (List.length results, Slice.list ~strict: false slice results)

  let search' = Lwt.map snd % search Slice.everything
  let count = Lwt.map fst % search Slice.nothing

  let tiebreakers = M.tiebreakers
end