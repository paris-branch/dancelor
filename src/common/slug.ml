
type t = string

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
