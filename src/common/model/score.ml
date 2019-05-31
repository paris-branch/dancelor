type 'a t =
  { score : float ;
    value : 'a }
[@@deriving yojson]

let from_value value = { score = 1. ; value }

let value s = s.value

let list_from_values s = List.map from_value s
let list_filter p =
  Lwt_list.filter_s (fun score -> p score.value)

let list_filter_threshold threshold =
  List.filter (fun score -> score.score >= threshold)

let list_sort_decreasing cmp =
  List.sort
    (fun score1 score2 ->
       let c = - compare score1.score score2.score in
       if c = 0 then
         cmp score1.value score2.value
       else
         c)

let list_map_score f (l : 'a t list) : 'a t list Lwt.t =
  Lwt_list.map_s
    (fun (score : 'a t) ->
       let%lwt new_score = f score.value in
       Lwt.return
         { score = score.score *. new_score ;
           value = score.value })
    l