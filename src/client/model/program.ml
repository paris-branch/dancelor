open Nes
include Dancelor_common_model.Program

let sets = sets >=>| Lwt_list.map_p Set.get

let warnings p =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in
  (* Check that there are no duplicate sets. *)
  let%lwt sets = sets p in
  let sets = List.sort Stdlib.compare sets in
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
  let tune_groups = List.sort Stdlib.compare tune_groups in
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
  Madge_client.(
    call ~endpoint:Endpoint.get @@ fun {a} _ ->
    a Arg.slug slug
  )

let get_all () =
  Madge_client.call ~endpoint:Endpoint.get_all @@ fun _ _ -> ()

let search ?pagination ?threshold string =
  Madge_client.(
    call ~endpoint:Endpoint.search @@ fun {a} {o} ->
    o Arg.pagination pagination;
    o Arg.threshold threshold;
    a Arg.string string
  )
