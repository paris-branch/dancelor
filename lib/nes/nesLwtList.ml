type 'a t = 'a list Lwt.t

let cons h l =
  let%lwt l' = l in
  Lwt.return (h::l')

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
