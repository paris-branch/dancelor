include Filename

let concat3 a b c =
  concat (concat a b) c

let concat4 a b c d =
  concat (concat a b) (concat c d)

let concat_l = function
  | [] -> failwith "Filename.concat_l"
  | h :: t -> List.fold_left concat h t
