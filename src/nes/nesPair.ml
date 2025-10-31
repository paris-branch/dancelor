let map_fst f (x, y) = (f x, y)
let map_snd g (x, y) = (x, g y)
let map f g (x, y) = (f x, g y)
let map_both f = map f f
let map2 f g (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

let cons x y = (x, y)
let snoc y x = (x, y)

let map_fst_lwt f (x, y) = let%lwt x = f x in Lwt.return (x, y)
