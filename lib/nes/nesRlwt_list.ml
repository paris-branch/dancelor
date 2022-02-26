type ('a, 'e) t = ('a list, 'e) NesRlwt.t

let rec iter f = function
  | [] -> NesRlwt.return ()
  | x::xs -> NesRlwt.bind (f x) (fun () -> iter f xs)
