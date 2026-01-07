open Nes
open Common

let get env id =
  match Database.Any.get id with
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

let cache = Cache.create ~lifetime: 600 ()

let search' env filter =
  Cache.use ~cache ~key: (env, filter) @@ fun () ->
  let (book_f, dance_f, person_f, set_f, source_f, tune_f, version_f) = Filter.Any.specialise filter in
  let%lwt (count_sources, sources) = Source.search' env source_f in
  let%lwt (count_persons, persons) = Person.search' env person_f in
  let%lwt (count_dances, dances) = Dance.search' env dance_f in
  let%lwt (count_books, books) = Book.search' env book_f in
  let%lwt (count_sets, sets) = Set.search' env set_f in
  let%lwt (count_tunes, tunes) = Tune.search' env tune_f in
  let%lwt (count_versions, versions) = Version.search' env version_f in
  let count = count_sources + count_persons + count_dances + count_books + count_sets + count_tunes + count_versions in
  let results =
    lwt_stream_merge_sorted_l (fun (_, s1) (_, s2) -> Float.compare s2 s1) [
      (* NOTE: keep this list's order in sync with Model.Any.Type.compare *)
      Lwt_stream.map (Pair.map_fst Model.Any.person) (Lwt_stream.of_seq persons);
      Lwt_stream.map (Pair.map_fst Model.Any.dance) (Lwt_stream.of_seq dances);
      Lwt_stream.map (Pair.map_fst Model.Any.source) (Lwt_stream.of_seq sources);
      Lwt_stream.map (Pair.map_fst Model.Any.tune) (Lwt_stream.of_seq tunes);
      Lwt_stream.map (Pair.map_fst Model.Any.version) (Lwt_stream.of_seq versions);
      Lwt_stream.map (Pair.map_fst Model.Any.set) (Lwt_stream.of_seq sets);
      Lwt_stream.map (Pair.map_fst Model.Any.book) (Lwt_stream.of_seq books);
    ]
  in
  lwt (count, results)

let search env slice filter =
  let%lwt (count, results) = search' env filter in
  Pair.cons count <$> (Lwt_stream.to_list @@ slice_lwt_stream ~strict: false slice @@ Lwt_stream.map fst results)

let search_context env filter element =
  let%lwt results = snd <$> search env Slice.everything filter in
  let List.{total; previous; index; next; _} = Option.get @@ List.find_context (Model.Any.equal element) results in
  (* TODO: Return the context directly. *)
  lwt (total, previous, index, next)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Any.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search env
  | SearchContext -> search_context env
