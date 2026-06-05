include Lwt_stream

let get_available_1 stream =
  NesList.to_option (get_available_up_to 1 stream)

let choose_biased =
  (* Given a list of streams, attempt to get a value from them in a
     non-blocking way. If they have no value, check whether they are
     closed, and filter them out. Return the list of streams that are
     still in the game. *)
  let get_first_available streams =
    let rec aux prev_streams = function
      | [] -> Error (List.rev prev_streams)
      | stream :: streams ->
        match get_available_1 stream with
        | Some x -> Ok (x, List.rev_append prev_streams streams)
        | None when is_closed stream -> aux prev_streams streams
        | None -> aux (stream :: prev_streams) streams
    in
    aux [] streams
  in
  let rec next_biased streams =
    match get_first_available streams with
    | Ok (x, streams) -> Lwt.return_some (x, streams)
    | Error [] -> Lwt.return_none
    | Error streams ->
      (* wait for at least one element to be available *)
      let%lwt _ = Lwt.pick (List.map peek streams) in
      next_biased streams
  in
  fun streams ->
    let streams = ref streams in
    Lwt_stream.from @@ fun () ->
    match%lwt next_biased !streams with
    | None -> Lwt.return_none
    | Some (x, streams') -> streams := streams'; Lwt.return_some x

type 'a next = Next of 'a | Last of 'a

let from_next f =
  let last_passed = ref false in
  from @@ fun () ->
  if !last_passed then Lwt.return_none
  else
    match%lwt f () with
    | Next x -> Lwt.return_some x
    | Last x -> last_passed := true; Lwt.return_some x

let return_lwt' promise =
  let first = ref true in
  from @@ fun () ->
  if !first then
    (
      let%lwt result = promise in
      first := false;
      Lwt.return_some result
    )
  else
    Lwt.return_none

let flip_lwt promise =
  concat (return_lwt' promise)
