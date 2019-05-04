include List

let rec map_filter f = function
  | [] -> []
  | h :: t ->
     match f h with
     | None -> map_filter f t
     | Some x -> x :: map_filter f t

(* type ('a, 'b) choice = A of 'a | B of 'b

let map_partition f l =
  let rec map_partition as_ bs = function
    | [] -> (List.rev as_, List.rev bs)
    | h :: t ->
       match f h with
       | A a -> map_partition (a :: as_) bs t
       | B b -> map_partition as_ (b :: bs) t
  in
  map_partition [] [] l *)

(* let pp ?(sep="; ") pp_x fmt = function
  | [] -> ()
  | x :: xs ->
     pp_x fmt x;
     List.iter (Format.fprintf fmt "%s%a" sep pp_x) xs *)

let rec sub n l =
  if n <= 0 then []
  else begin
    match l with
    | [] -> []
    | h::t -> h::(sub (n-1) t)
  end
