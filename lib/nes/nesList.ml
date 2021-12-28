include List

let rec map_filter f = function
  | [] -> []
  | h :: t ->
     match f h with
     | None -> map_filter f t
     | Some x -> x :: map_filter f t

let rec sub n l =
  if n <= 0 then []
  else begin
    match l with
    | [] -> []
    | h::t -> h::(sub (n-1) t)
  end

let hd_opt = function
  | [] -> None
  | h :: _ -> Some h

let bd l =
  let rec bd acc = function
    | [] -> failwith "bd"
    | [_] -> List.rev acc
    | h::q -> bd (h::acc) q
  in
  bd [] l

let rec ft = function
  | [] -> failwith "ft"
  | [e] -> e
  | _::q -> ft q

let intertwine f l =
  let rec intertwine i = function
    | [] -> []
    | [e] -> [e]
    | h :: q ->
      h :: (f i) :: intertwine (i+1) q
  in
  intertwine 0 l

let sort_uniq_count cmp l =
  (* count_duplicates assumes that the list is sorted *)
  let rec count_duplicates acc (prev, nb) = function
    | [] -> rev ((prev, nb) :: acc)
    | h :: t ->
      if cmp h prev = 0 then
        count_duplicates acc (prev, nb+1) t
      else
        count_duplicates ((prev,nb)::acc) (h,1) t
  in
  match sort cmp l with
  | [] -> []
  | h :: t -> count_duplicates [] (h,1) t
