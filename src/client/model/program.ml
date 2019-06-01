include Dancelor_common_model.Program

let sets p =
  let%lwt sets = sets p in
  Lwt_list.map_p
    (fun slug ->
       Dancelor_client_api.request
         ~route:(Dancelor_common.Router.Set slug)
         ~reader:Set.of_yojson
         ())
    sets

let warnings p =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in
  (* Check that there are no duplicate sets. *)
  let%lwt sets = sets p in
  let sets = List.sort Pervasives.compare sets in
  (match sets with
   | [] -> add_warning Empty
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
  (* Check that there are no duplicate tunes. *)
  let%lwt tunes = Lwt_list.map_s Set.tunes sets in
  let tunes = List.flatten tunes in
  let%lwt tune_groups = Lwt_list.map_s Tune.group tunes in
  let tune_groups = List.sort Pervasives.compare tune_groups in
  (match tune_groups with
   | [] -> ()
   | tune_group :: tune_groups ->
     let _ =
       List.fold_left
         (fun prev curr ->
            if prev = curr then
              add_warning (DuplicateTune curr);
            curr)
         tune_group
         tune_groups
     in
     ());
  Lwt.return !warnings

(* * *)

let get slug =
  Dancelor_client_api.request
    ~route:(Dancelor_common.Router.Program slug)
    ~reader:of_yojson
    ()

let get_all () =
  Dancelor_client_api.request
    ~route:Dancelor_common.Router.ProgramAll
    ~reader:(Dancelor_common.Unserializer.list of_yojson)
    ()
