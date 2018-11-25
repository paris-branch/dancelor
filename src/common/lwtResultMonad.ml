type ('a, 'b) m = ('a, 'b) result Lwt.t

let bind (x : ('a, 'b) m) (f : 'a -> ('c, 'b) result Lwt.t) : ('c, 'b) result Lwt.t =
  Lwt.bind x (function
      | Ok v -> f v
      | Error e -> Lwt.return (Error e))

let (>>=) = bind
