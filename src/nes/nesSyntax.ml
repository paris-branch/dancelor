let (||>) f g x = f x |> g

let (>>=|) = NesLwt.bind
let (>=>|) = NesLwt.compose
let (>|=|) p f = NesLwt.map f p

let (>>=?|) x f = match%lwt x with Some x -> f x | _ -> Lwt.return_none
let (>=>?|) f g x = (f x) >>=?| g
let (<&>?|) x f = match%lwt x with Some x -> Lwt.return_some (f x) | _ -> Lwt.return_none
