open NesUnix
open Common

module Ly = Ly
module Svg = Svg
module Ogg = Ogg
module Pdf = Pdf

let get = Model.Version.get

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
  type value = Model.Version.t Entry.t
  type filter = Model.Version.Filter.t

  let cache = Cache.create ~lifetime: 600 ()
  let get_all = Database.Version.get_all
  let filter_accepts = Model.Version.Filter.accepts

  let tiebreakers =
    Lwt_list.[increasing (Lwt.map Model.Tune.name % Model.Version.tune) String.Sensible.compare]
end)

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun _env endpoint ->
  match endpoint with
  | Get -> get
  | Search -> search
  | Create -> create
  | Update -> update
  | Ly -> Ly.get
  | Svg -> Svg.get
  | Ogg -> Ogg.get
  | Pdf -> Pdf.get
  | PreviewSvg -> Svg.preview
  | PreviewOgg -> Ogg.preview
