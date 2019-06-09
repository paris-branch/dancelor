open Nes
include Dancelor_common_model.Search

type model =
  | Person of Person.t
  | Credit of Credit.t
  | Tune of Tune.t
  | TuneGroup of TuneGroup.t
  | Program of Program.t
  | Set of Set.t
  | Dance of Dance.t
  | Source of Source.t

let unperson = function Person p -> Some p | _ -> None
let uncredit = function Credit c -> Some c | _ -> None
let untune = function Tune t -> Some t | _ -> None
let untune_group = function TuneGroup g -> Some g | _ -> None
let unprogram = function Program p -> Some p | _ -> None
let unset = function Set s -> Some s | _ -> None
let undance = function Dance d -> Some d | _ -> None
let unsource = function Source s -> Some s | _ -> None

let search ?pagination ?threshold string =
  (* FIXME: add others *)
  let%lwt persons = Person.search ?pagination ?threshold string in
  let%lwt credits = Credit.search ?pagination ?threshold string in
  let%lwt tunes = Tune.search ?pagination ?threshold string in
  let%lwt tune_groups = TuneGroup.search ?pagination ?threshold string in
  let%lwt programs = Program.search ?pagination ?threshold string in
  let%lwt sets = Set.search ?pagination ?threshold string in
  let%lwt dances = Dance.search ?pagination ?threshold string in
  let%lwt sources = Source.search ?pagination ?threshold string in
  let all =
    Score.list_map (fun p -> Person p) persons
    @ Score.list_map (fun c -> Credit c) credits
    @ Score.list_map (fun t -> Tune t) tunes
    @ Score.list_map (fun g -> TuneGroup g) tune_groups
    @ Score.list_map (fun p -> Program p) programs
    @ Score.list_map (fun s -> Set s) sets
    @ Score.list_map (fun d -> Dance d) dances
    @ Score.list_map (fun s -> Source s) sources
  in
  let all = Score.list_sort_decreasing all in
  let%lwt all =
    Option.unwrap_map_or
      Lwt.return Pagination.apply pagination
      all
  in
  (* Quelle horreur : *)
  let persons = Score.list_map_filter unperson all in
  let%lwt persons = Score.list_map_lwt_s Person.slug persons in
  let m_persons = Option.unwrap_map_or 0. Score.score (List.hd_opt persons) in
  let credits = Score.list_map_filter uncredit all in
  let%lwt credits = Score.list_map_lwt_s Credit.slug credits in
  let m_credits = Option.unwrap_map_or 0. Score.score (List.hd_opt credits) in
  let tunes = Score.list_map_filter untune all in
  let%lwt tunes = Score.list_map_lwt_s Tune.slug tunes in
  let m_tunes = Option.unwrap_map_or 0. Score.score (List.hd_opt tunes) in
  let tune_groups = Score.list_map_filter untune_group all in
  let%lwt tune_groups = Score.list_map_lwt_s TuneGroup.slug tune_groups in
  let m_tune_groups = Option.unwrap_map_or 0. Score.score (List.hd_opt tune_groups) in
  let programs = Score.list_map_filter unprogram all in
  let%lwt programs = Score.list_map_lwt_s Program.slug programs in
  let m_programs = Option.unwrap_map_or 0. Score.score (List.hd_opt programs) in
  let sets = Score.list_map_filter unset all in
  let%lwt sets = Score.list_map_lwt_s Set.slug sets in
  let m_sets = Option.unwrap_map_or 0. Score.score (List.hd_opt sets) in
  let dances = Score.list_map_filter undance all in
  let%lwt dances = Score.list_map_lwt_s Dance.slug dances in
  let m_dances = Option.unwrap_map_or 0. Score.score (List.hd_opt dances) in
  let sources = Score.list_map_filter unsource all in
  let%lwt sources = Score.list_map_lwt_s Source.slug sources in
  let m_sources = Option.unwrap_map_or 0. Score.score (List.hd_opt sources) in
  Lwt.return
    Results.{
      persons = (m_persons, persons) ;
      credits = (m_credits, credits) ;
      tunes = (m_tunes, tunes) ;
      tune_groups = (m_tune_groups, tune_groups) ;
      programs = (m_programs, programs) ;
      sets = (m_sets, sets) ;
      dances = (m_dances, dances) ;
      sources = (m_sources, sources) ;
    }
