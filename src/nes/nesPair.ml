let fst (x, _) = x
let snd (_, y) = y

let set_fst x (_, y) = (x, y)
let set_snd y (x, _) = (x, y)

let map_fst f (x, y) = (f x, y)
let map_snd g (x, y) = (x, g y)
let map f g (x, y) = (f x, g y)
let map_both f = map f f
let map2 f g (x1, y1) (x2, y2) = (f x1 x2, g y1 y2)

let cons x y = (x, y)
let snoc y x = (x, y)
