open Protocol_conv_jsonm
open Protocol_conv_yaml

type t = string
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

let of_string s =
  let s = String.lowercase_ascii s in
  let l = String.length s in
  let _b = Bytes.create l in
  let _j = ref 0 in
  let _p = ref true in
  for i = 0 to l - 1 do
    let _c = s.[i] in
    ()
  done;
  ""

let%test _ = of_string "Hello you, how are you?!" = "hello-you-how-are-you"
let%test _ = of_string "<> My friend!" = "my-friend"
