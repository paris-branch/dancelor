open Nes
include Dancelor_common_model.Program

let sets_and_parameters program =
  let%lwt sets_and_parameters = sets_and_parameters program in
  Lwt_list.map_p
    (fun (slug, parameters) ->
       let%lwt set = Set.get slug in
       Lwt.return (set, parameters))
    sets_and_parameters

let warnings p =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in
  (* Check that there are no duplicate sets. *)
  let%lwt sets_and_parameters = sets_and_parameters p in
  let sets = List.map fst sets_and_parameters in
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
  (* Check that there are no duplicate versions. *)
  let%lwt versions_and_parameters = Lwt_list.map_s Set.versions_and_parameters sets in
  let versions_and_parameters = List.flatten versions_and_parameters in
  let versions = List.map fst versions_and_parameters in
  let%lwt tunes = Lwt_list.map_s Version.tune versions in
  let tunes = List.sort Stdlib.compare tunes in
  (match tunes with
   | [] -> ()
   | tune :: tunes ->
     let _ =
       List.fold_left
         (fun prev curr ->
            if prev = curr then
              add_warning (DuplicateVersion curr);
            curr)
         tune
         tunes
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
