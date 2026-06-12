include Lwt_stream

let get_available_1 stream =
  NesList.to_option (get_available_up_to 1 stream)

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
