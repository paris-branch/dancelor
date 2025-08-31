include Lwt_stream

let get_available_1 stream =
  NesList.to_option (get_available_up_to 1 stream)

let choose_biased streams =
  Lwt_stream.from @@ fun () ->
  (* try to find an element in the streams in a blocking way. if it does not
     work, then wait for the first element to show up in any stream *)
  Lwt.map Option.some @@
    match List.find_map get_available_1 streams with
    | Some x -> Lwt.return x
    | None -> Lwt.pick (List.map next streams)

type 'a next = Next of 'a | Last of 'a | None

let from_next f =
  let last_passed = ref false in
  from @@ fun () ->
  if !last_passed then Lwt.return_none
  else
    match%lwt f () with
    | Next x -> Lwt.return_some x
    | Last x -> last_passed := true; Lwt.return_some x
    | None -> Lwt.return_none
