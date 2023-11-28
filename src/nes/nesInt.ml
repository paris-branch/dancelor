include Int

let to_english_string n =
  NesInt64.to_english_string (Int64.of_int n)

let to_english_string_times n =
  NesInt64.to_english_string_times (Int64.of_int n)

let compare_lwt n1 n2 = Lwt.return (compare n1 n2)
