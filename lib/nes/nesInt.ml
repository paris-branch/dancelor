include Int

let to_english_string n =
  NesInt64.to_english_string (Int64.of_int n)

let to_english_times_string n =
  NesInt64.to_english_times_string (Int64.of_int n)
