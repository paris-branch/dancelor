open Nes
include Dancelor_common_model.Book

let contents p =
  let%lwt contents = contents p in
  Lwt_list.map_p
    (function
      | (Version (v, p) : Self.page) -> let%lwt v = Version.get v in Lwt.return (Version (v, p))
      | Set (s, p) -> let%lwt s = Set.get s in Lwt.return (Set (s, p))
      | InlineSet (s, p) -> Lwt.return (InlineSet (s, p)))
    contents

let warnings p =
  let warnings = ref [] in
  let add_warning w = warnings := w :: !warnings in

  let%lwt contents = contents p in

  (* Check that there are no duplicate sets. *)
  let%lwt sets =
    Lwt_list.filter_map_p
      (function
        | Version _ -> Lwt.return_none
        | Set (s, _) | InlineSet (s, _) -> Lwt.return_some s)
      contents
  in
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

  (* Check that there are no duplicate tune. *)
  let%lwt standalone_versions =
    Lwt_list.filter_map_p
      (function
        | Version (v, _) -> Lwt.return_some v
        | _ -> Lwt.return_none)
      contents
  in
  let%lwt contained_versions =
    let%lwt versions_and_parameters = Lwt_list.map_s Set.versions_and_parameters sets in
    let versions_and_parameters = List.flatten versions_and_parameters in
    Lwt.return (List.map fst versions_and_parameters)
  in
  let versions = standalone_versions @ contained_versions in
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

  (* Return *)
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
