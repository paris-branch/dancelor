open Protocol_conv_jsonm
open Protocol_conv_yaml

type t = string
[@@deriving protocol ~driver:(module Jsonm),
            protocol ~driver:(module Yaml)]

let from_string str =
  let str = String.lowercase_ascii str in
  let len = String.length str in
  let out = Bytes.create len in
  let j = ref 0 in
  let last_letter = ref (-10) in
  for i = 0 to len - 1 do
    if 'a' <= str.[i] && str.[i] <= 'z' then
      (
        Bytes.set out !j str.[i];
        last_letter := !j;
        incr j
      )
    else if !last_letter = !j - 1 then
      (
        Bytes.set out !j '-';
        incr j
      )
  done;
  Bytes.sub_string out 0 (!last_letter+1)

let%test _ = from_string "Hello you, how are you?!" = "hello-you-how-are-you"

let%test _ = from_string "<> My friend!" = "my-friend"
