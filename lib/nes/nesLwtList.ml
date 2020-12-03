type 'a t = 'a list Lwt.t

let cons h l =
  let%lwt l' = l in
  Lwt.return (h::l')

let rec merge uniq compare = function
  | [], l | l, [] ->
    Lwt.return l
  | h1::t1, h2::t2 ->
    let%lwt cmp = compare h1 h2 in
    if cmp < 0 then cons h1 (merge uniq compare (t1,h2::t2))
    else if cmp = 0 && uniq then cons h1 (merge uniq compare (t1,t2))
    else if cmp = 0 then cons h1 (merge uniq compare (t1,h2::t2))
    else cons h2 (merge uniq compare (h1::t1,t2))

let rec split = function
  | [] -> [], []
  | [h] -> [h],[]
  | h1::h2::t ->
    let l1,l2 = split t in
    h1::l1,h2::l2

let sort_param uniq compare l =
  let%lwt l = l in
  let rec sort_aux = function
    | [] -> Lwt.return []
    | [h] -> Lwt.return [h]
    | l ->
      let l1,l2 = split l in
      let%lwt l1s = sort_aux l1 in
      let%lwt l2s = sort_aux l2 in
      merge uniq compare (l1s,l2s)
  in
  sort_aux l

let sort compare l = sort_param false compare l

let sort_uniq compare l = sort_param true compare l

let map f l =
  Lwt.map (List.map f) l

let resolve (l : 'a Lwt.t t) : 'a t =
  Lwt.bind l (Lwt_list.map_p (fun p -> p))

let sub n (l : 'a t) : 'a t =
  let rec aux = function
    | 0,_ -> []
    | _,[] -> []
    | n,h::t -> h::(aux (n-1,t))
  in
  Lwt.bind l (fun l -> Lwt.return (aux (n,l)))
