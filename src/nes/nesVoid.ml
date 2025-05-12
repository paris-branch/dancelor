type t = |

let f _ = assert false

let yojson_of_t = f
let t_of_yojson j = NesJson.of_yojson_error "there are no void values" j
