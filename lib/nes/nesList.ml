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
