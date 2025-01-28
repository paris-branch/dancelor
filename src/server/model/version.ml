open Nes
open Common

include ModelBuilder.Version.Build(Person)(Tune)

let get = Database.Version.get

let create = Database.Version.create
let update = Database.Version.update
let save = Database.Version.save

let rec search_and_extract acc s regexp =
  let rem = Str.replace_first regexp "" s in
  try
    let gp = Str.matched_group 1 s in
    let gp_words =
      String.split_on_char ',' gp
      |> List.map (String.remove_char '"')
      |> List.map (String.remove_char '\'')
      |> List.filter (fun s -> s <> "")
    in
    let rem, l = search_and_extract acc rem regexp in
    rem, gp_words @ l
  with
  | Not_found | Invalid_argument _ -> rem, acc

let score_list_vs_word words needle =
  List.map (String.inclusion_proximity ~char_equal: Char.Sensible.equal ~needle) words
  |> List.fold_left max 0.

let score_list_vs_list words needles =
  if needles = [] then 1.
  else
    begin
      List.map (score_list_vs_word words) needles
      |> List.fold_left max 0.
    end

include ModelBuilder.Search.Build(struct
    type value = t Entry.t
    type filter = Filter.t

    let cache = Cache.create ~lifetime: 600 ()
    let get_all = Database.Version.get_all
    let filter_accepts = Filter.accepts

    let tiebreakers =
      Lwt_list.[
        increasing (Lwt.map Tune.name % tune) String.Sensible.compare
      ]
  end)

module Parameters = struct
  include ModelBuilder.Version.Parameters

  let for_dance p =
    let%olwt dance_slug = Lwt.return (for_dance p) in
    let%lwt dance = Dance.get dance_slug in
    Lwt.return_some dance
end
