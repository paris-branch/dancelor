include Filename

let concat_l = function
  | [] -> failwith "Filename.concat_l"
  | h :: t -> List.fold_left concat h t
