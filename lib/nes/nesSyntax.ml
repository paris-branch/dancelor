let ( ||> ) f g x = f x |> g
let ( @@@ ) f g x = f (g x)

(* FIXME: this has to be renamed and go in the Result syntax *)
let ( @@@@ ) f g x =
  match g x with
  | Ok y -> Ok (f y)
  | Error err -> Error err

let ( >>=? ) = NesOption.bind
let ( >=>? ) = NesOption.compose

let ( >>=| ) = NesLwt.bind
let ( >=>| ) = NesLwt.compose
let ( >|=| ) p f = NesLwt.map f p

let ( >>=?| ) x f = match%lwt x with Some x -> f x | _ -> Lwt.return_none
let ( >=>?| ) f g x = (f x) >>=?| g
let ( <&>?| ) x f = match%lwt x with Some x -> Lwt.return_some (f x) | _ -> Lwt.return_none
