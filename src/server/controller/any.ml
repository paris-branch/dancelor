open Nes
open Dancelor_common
open Model_new

let get env id =
  match%lwt Database.Any.get id with
  | None -> Permission.reject_can_get ()
  | Some any ->
    Model.Any.to_entry'
      any
      ~on_public: (Permission.assert_can_get_public env)
      ~on_private: (Permission.assert_can_get_private env);%lwt
    lwt any

(** Given two streams sorted according to the comparison function, produce one
    sorted stream of all the values. In case of equality, the left stream wins. *)
let lwt_stream_merge_sorted cmp xs ys =
  Lwt_stream.from @@ fun () ->
  let%lwt x = Lwt_stream.peek xs in
  let%lwt y = Lwt_stream.peek ys in
  match x, y with
  | Some x, Some y when cmp x y <= 0 -> Lwt_stream.junk xs;%lwt lwt_some x
  | Some _, Some y -> Lwt_stream.junk ys;%lwt lwt_some y
  | Some x, _ -> Lwt_stream.junk xs;%lwt lwt_some x
  | _, Some y -> Lwt_stream.junk ys;%lwt lwt_some y
  | _ -> lwt_none

(** Given a list of streams sorted according to the comparison function, produce
    one sorted stream of all the values. In case of equality, a stream appearing
    earlier in the list wins. *)
let lwt_stream_merge_sorted_l cmp = function
  | [] -> Lwt_stream.of_list []
  | s :: ss -> List.fold_left (lwt_stream_merge_sorted cmp) s ss

(** Slice a stream. Raises {!Invalid_argument} if [start] is strictly bigger
    than the length of the stream. If [strict] is set (the default), also raises
    {!Invalid_argument} if [end_excl] is strictly bigger than the length of the
    stream; otherwise, silently include everything until the end of the
    stream. *)
let slice_lwt_stream = fun ?(strict = true) slice xs ->
  let i = ref 0 in
  let rec next () =
    match%lwt Lwt_stream.get xs with
    | None when strict && Slice.end_incl slice <> !i - 1 -> invalid_arg "Slice.stream"
    | Some _ when Slice.start slice > !i -> incr i; next ()
    | Some x when Slice.end_incl slice >= !i -> incr i; Lwt.return_some x
    | _ -> Lwt.return_none
  in
  Lwt_stream.from next

let cache : (Environment.cache_key * Filter.Any.t, (int * (Model_new.Any_row.t * float) list) Lwt.t) Cache.t = Cache.create ~lifetime: 600 ()

let search' env filter =
  Cache.use ~cache ~key: (Environment.cache_key env, filter) @@ fun () ->
  let (book_f, dance_f, person_f, set_f, source_f, tune_f, version_f) = Filter.Any.specialise filter in
  let%lwt persons_result = Person.search' env person_f in
  let%lwt dances_result = Dance.search' env dance_f in
  let%lwt sources_result = Source.search' env source_f in
  let%lwt books_result = Book.search' env book_f in
  let%lwt sets_result = Set.search' env set_f in
  let%lwt tunes_result = Tune.search' env tune_f in
  let%lwt versions_result = Version.search' env version_f in
  let count = sources_result.total + persons_result.total + dances_result.total + books_result.total + sets_result.total + tunes_result.total + versions_result.total in
  let results =
    let stream_to_row to_row =
      Lwt_stream.map (Pair.map_fst to_row)
    in
    let stream_to_row_s to_row =
      Lwt_stream.map_s (Monadise_lwt.monadise_1_1 Pair.map_fst to_row)
    in
    lwt_stream_merge_sorted_l (fun (_, s1) (_, s2) -> Float.compare s2 s1) [
      (* NOTE: keep this list's order in sync with Model.Any.Type.compare *)
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.person) (stream_to_row Person.to_row (Lwt_stream.of_list persons_result.items));
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.dance) (stream_to_row_s Dance.to_row (Lwt_stream.of_list dances_result.items));
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.source) (stream_to_row_s Source.to_row (Lwt_stream.of_list sources_result.items));
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.tune) (stream_to_row_s Tune.to_row (Lwt_stream.of_list tunes_result.items));
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.version) (stream_to_row_s Version.to_row (Lwt_stream.of_list versions_result.items));
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.set) (stream_to_row_s (Set.to_row env) (Lwt_stream.of_list sets_result.items));
      Lwt_stream.map (Pair.map_fst Model_new.Any_row.book) (stream_to_row_s (Book.to_row env) (Lwt_stream.of_list books_result.items));
    ]
  in
  let%lwt results = Lwt_stream.to_list results in
  lwt (count, results)

let search env slice filter =
  let%lwt (total, items) = search' env filter in
  let items = Slice.list ~strict: false slice @@ List.map fst items in
  lwt {Model_new.total; items}

let search_context env filter element =
  let%lwt results = items <$> search env Slice.everything filter in
  match List.find_context (Any_id.equal element) (List.map Any_row.to_id results) with
  | None -> Madge_server.shortcut_not_found "Could not find the given element in the search results."
  | Some List.{total; previous; index; next; _} -> lwt {Model_new.index; total; previous_item = previous; next_item = next}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Any.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | Search_context -> search_context env
