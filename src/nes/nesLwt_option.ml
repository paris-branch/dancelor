let bind x f =
  Lwt.bind x @@ function
    | None -> Lwt.return_none
    | Some x -> f x

let bind' x f =
  match x with
  | None -> Lwt.return_none
  | Some x -> f x
