let unit = function
  | `Assoc[] -> Ok ()
  | _ -> Error "not unit"

let list unserializer = function
  | `List l ->
    ( let l =
      List.fold_left
        (fun l e ->
          match l with
          | Error s -> Error s
          | Ok l ->
            match unserializer e with
            | Error s -> Error s
            | Ok e -> Ok (e :: l))
        (Ok [])
        l
    in
    match l with
    | Ok l -> Ok (List.rev l)
    | Error s -> Error s)
  | _ -> Error "not a list"
