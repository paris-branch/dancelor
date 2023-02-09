include Lwt

let compose f g = fun x -> bind (f x) g

module Syntax = struct
  let ( >>=| ) = bind
  let ( >=>| ) = compose
  let ( >|=| ) p f = map f p
end
