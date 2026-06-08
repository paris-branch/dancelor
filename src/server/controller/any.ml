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

let get_rows env ids =
  let (person_ids, dance_ids, source_ids, tune_ids, version_ids, set_ids, book_ids) =
    List.fold_left
      (fun
          (person_ids, dance_ids, source_ids, tune_ids, version_ids, set_ids, book_ids)
          id
        ->
        match id with
        | Any_id.Person id -> (id :: person_ids, dance_ids, source_ids, tune_ids, version_ids, set_ids, book_ids)
        | Dance id -> (person_ids, id :: dance_ids, source_ids, tune_ids, version_ids, set_ids, book_ids)
        | Source id -> (person_ids, dance_ids, id :: source_ids, tune_ids, version_ids, set_ids, book_ids)
        | Tune id -> (person_ids, dance_ids, source_ids, id :: tune_ids, version_ids, set_ids, book_ids)
        | Version id -> (person_ids, dance_ids, source_ids, tune_ids, id :: version_ids, set_ids, book_ids)
        | Set id -> (person_ids, dance_ids, source_ids, tune_ids, version_ids, id :: set_ids, book_ids)
        | Book id -> (person_ids, dance_ids, source_ids, tune_ids, version_ids, set_ids, id :: book_ids)
      )
      ([], [], [], [], [], [], [])
      ids
  in
  let%lwt person_rows = Person.get_rows_table env person_ids in
  let%lwt dance_rows = Dance.get_rows_table env dance_ids in
  let%lwt source_rows = Source.get_rows_table env source_ids in
  let%lwt tune_rows = Tune.get_rows_table env tune_ids in
  let%lwt version_rows = Version.get_rows_table env version_ids in
  let%lwt set_rows = Set.get_rows_table env set_ids in
  let%lwt book_rows = Book.get_rows_table env book_ids in
  lwt @@
    List.filter_map
      (function
        | Any_id.Person id -> Option.map Any_row.person @@ Hashtbl.find_opt person_rows id
        | Dance id -> Option.map Any_row.dance @@ Hashtbl.find_opt dance_rows id
        | Source id -> Option.map Any_row.source @@ Hashtbl.find_opt source_rows id
        | Tune id -> Option.map Any_row.tune @@ Hashtbl.find_opt tune_rows id
        | Version id -> Option.map Any_row.version @@ Hashtbl.find_opt version_rows id
        | Set id -> Option.map Any_row.set @@ Hashtbl.find_opt set_rows id
        | Book id -> Option.map Any_row.book @@ Hashtbl.find_opt book_rows id
      )
      ids

(** Given two streams sorted according to the comparison function, produce one
    sorted stream of all the values. In case of equality, the left stream wins. *)
let lwt_stream_merge_sorted cmp xs ys =
  Lwt_stream.from @@ fun () ->
  let%lwt x = Lwt_stream.peek xs in
  let%lwt y = Lwt_stream.peek ys in
  match x, y with
  | Some x, Some y when cmp x y <= 0 -> Lwt_stream.junk xs;%lwt lwt_some x
  | Some _, Some y -> Lwt_stream.junk ys;%lwt lwt_some y
  | Some x, None -> Lwt_stream.junk xs;%lwt lwt_some x
  | None, Some y -> Lwt_stream.junk ys;%lwt lwt_some y
  | None, None -> lwt_none

(** Given a list of streams sorted according to the comparison function, produce
    one sorted stream of all the values. In case of equality, a stream appearing
    earlier in the list wins. *)
let lwt_stream_merge_sorted_l cmp = function
  | [] -> Lwt_stream.of_list []
  | s :: ss -> List.fold_left (lwt_stream_merge_sorted cmp) s ss

(** Given two lists sorted according to the comparison function,
    produce one sorted list of all the values. In case of equality,
    the left list wins. *)
let list_merge_sorted_on cmp xs ys =
  let rec aux = function
    | [], [] -> []
    | xs, [] -> xs
    | [], ys -> ys
    | x :: xs, ((y :: _) as ys) when cmp x y <= 0 -> x :: aux (xs, ys)
    | xs, y :: ys -> y :: aux (xs, ys)
  in
  aux (xs, ys)

(** Given a list of lists sorted according to the comparison function,
    produce one sorted list of all the values. In case of equality, a
    list appearing earlier wins. *)
let list_merge_sorted_l_on cmp = function
  | [] -> []
  | l :: ls -> List.fold_left (list_merge_sorted_on cmp) l ls

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
      Lwt_stream.map_s (Monadise_lwt.lift_1_1 Pair.map_fst to_row)
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

let search_new env slice filter =
  let%lwt persons_result = Person.search'_new env filter in
  let total = persons_result.total in
  let items =
    list_merge_sorted_l_on (fun (_, s1) (_, s2) -> Float.compare s2 s1) [
      List.map (Pair.map_fst Any_row.person) persons_result.items;
    ]
  in
  let items = List.map fst @@ Slice.list ~strict: false slice items in
  lwt {Model_new.total; items}

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Any.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Get_rows -> get_rows env
  | Search -> search env
  | Search_context -> search_context env
  | Search_new -> search_new env
