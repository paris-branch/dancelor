open Nes
open Dancelor_common_model
include BookCore

let contents p =
  let%lwt contents = contents p in
  Lwt_list.map_p
    (function
      | (Version (v, p) : page_slug) -> let%lwt v = Version.get v in Lwt.return (Version (v, p))
      | Set (s, p) -> let%lwt s = Set.get s in Lwt.return (Set (s, p))
      | InlineSet (s, p) -> Lwt.return (InlineSet (s, p)))
    contents

let warnings p =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in

  let%lwt contents = contents p in

  (* Raise the [Empty] warning if there is nothing in this book *)
  if contents = [] then add_warning Empty;

  (* Check that there are no duplicate sets. *)
  let%lwt sets =
    Lwt_list.filter_map_p
      (function
        | Version _ -> Lwt.return_none
        | Set (s, _) | InlineSet (s, _) -> Lwt.return_some s)
      contents
  in
  (* FIXME: a clean comparison function for model objects (basically slug; and
     none = different) *)
  let sets = List.sort Stdlib.compare sets in
  (match sets with
   | [] -> ()
   | set :: sets ->
     let _ =
       List.fold_left
         (fun prev curr ->
            if prev = curr then
              add_warning (DuplicateSet curr);
            curr)
         set sets
     in
     ());

  (* remove duplicate sets to avoid further warnings *)
  (* FIXME: a clean comparison function for model objects (basically slug; and
     none = different) *)
  let sets = List.sort_uniq Stdlib.compare sets in

  (* Check that there are no duplicate tune. *)
  let%lwt standalone_versions =
    Lwt_list.filter_map_p
      (function
        | Version (v, _) -> Lwt.return_some v
        | _ -> Lwt.return_none)
      contents
  in

  (* Extend the list of sets associated to this tune. Creates it if it was not yet in the hashtable *)
  let extend htbl tune set_opt =
    match Hashtbl.find_opt htbl tune with
    | None -> Hashtbl.add htbl tune [set_opt]
    | Some sets -> Hashtbl.replace htbl tune (set_opt :: sets)
  in

  (* [tunes_to_set] is a hashtable from tunes to sets they belong to.
     Standalone tunes are associated with None *)
  let tunes_to_set = Hashtbl.create 8 in
  (* register standalone tunes *)
  Lwt_list.iter_s
    (fun v ->
       let%lwt tune = Version.tune v in
       extend tunes_to_set tune None;
       Lwt.return ())
    standalone_versions;%lwt
  (* register tunes in sets *)
  Lwt_list.iter_s
    (fun set ->
       let%lwt versions_and_parameters = Set.versions_and_parameters set in
       let versions = List.map fst versions_and_parameters in
       Lwt_list.iter_s
         (fun v ->
            let%lwt tune = Version.tune v in
            extend tunes_to_set tune (Some set);
            Lwt.return ())
         versions)
    sets;%lwt
  (* crawl all registered tunes and see if they appear several times. if that is
     the case, add a warning accordingly *)
  Hashtbl.iter
    (fun tune sets_opt ->
       if List.length sets_opt > 1 then
         (* FIXME: a clean comparison function for model objects (basically
            slug; and none = different). *)
         let sets_opt = List.sort_uniq_count (Option.compare Stdlib.compare) sets_opt in
         add_warning (DuplicateVersion (tune, sets_opt)))
    tunes_to_set;

  (* Return *)
  Lwt.return !warnings

let get the_slug =
  let open BookEndpoints in
  let open Arguments in
  Madge_client.(
    call ~endpoint:get @@ fun {a} _ ->
    a slug the_slug
  )
