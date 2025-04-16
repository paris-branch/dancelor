open NesUnix
open Common

module Ly = Ly
module Svg = Svg
module Ogg = Ogg
module Pdf = Pdf

let get env slug =
  Lwt.bind_return
    (Model.Version.get slug)
    (Permission.assert_can_get env)

let create env version =
  Permission.assert_can_create env;%lwt
  Database.Version.create version

let update env slug version =
  Lwt.bind (get env slug) (Permission.assert_can_update env);%lwt
  Database.Version.update slug version

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

let dispatch : type a r. Environment.t -> (a, r Lwt.t, r) Endpoints.Version.t -> a = fun env endpoint ->
  match endpoint with
  | Get -> get env
  | Search -> search (* FIXME: also filter search result *)
  | Create -> create env
  | Update -> update env
  | Ly -> Ly.get env
  | Svg -> Svg.get env
  | Ogg -> Ogg.get env
  | Pdf -> Pdf.get env
  | PreviewSvg -> Svg.preview env
  | PreviewOgg -> Ogg.preview env
