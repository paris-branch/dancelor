open Nes

type 'a t = {
  score: float;
  value: 'a;
}
[@@deriving yojson]

let score s = s.score

let score_to_string s =
  score s |> ( *. ) 100. |> int_of_float |> string_of_int

let from_value value = { score = 1.; value }

let value s = s.value

let lwt_map_from_list score =
  Lwt_list.map_s
    (
      fun value ->
        let%lwt score = score value in
        Lwt.return { score; value }
    )

let list_from_values s = List.map from_value s

let list_filter p =
  List.filter (fun score -> p score.value)

let list_filter_lwt p =
  Lwt_list.filter_s (fun score -> p score.value)

let list_filter_threshold threshold =
  List.filter (fun score -> score.score >= threshold)

let increasing = Lwt_list.increasing
let decreasing = Lwt_list.decreasing

let list_proj_sort_decreasing compares l =
  let compares =
    List.map
      (
        fun compare ->
          fun x y -> compare x.value y.value
      )
      compares
  in
  let compares =
    (decreasing (fun x -> Lwt.return x.score) compare) :: compares
  in
  Lwt_list.sort_multiple compares l

let list_sort_decreasing l = list_proj_sort_decreasing [] l

let list_map f =
  List.map (fun score -> { score with value = f score.value } )

let list_map_lwt_s f =
  Lwt_list.map_s
    (
      fun score ->
        let%lwt value = f score.value in
        Lwt.return { score with value }
    )

let list_map_lwt_p f =
  Lwt_list.map_p
    (
      fun score ->
        let%lwt value = f score.value in
        Lwt.return { score with value }
    )

let list_map_filter f =
  List.map_filter
    (
      fun score ->
        f score.value
        >>=? fun value ->
          Some { score with value }
    )

let list_map_score f (l : 'a t list) : 'a t list Lwt.t =
  Lwt_list.map_s
    (
      fun (score : 'a t) ->
        let%lwt new_score = f score.value in
        Lwt.return
          {
            score = score.score *. new_score;
            value = score.value
          }
    )
    l

let list_erase l =
  List.map (fun score -> score.value) l

module Make_Serialisable (M: Madge_common.SERIALISABLE) = struct
  type nonrec t = M.t t

  let _key = M._key ^ "-score"

  let of_yojson = of_yojson M.of_yojson
  let to_yojson = to_yojson M.to_yojson
end
