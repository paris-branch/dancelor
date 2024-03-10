open Dancelor_common_model

val search :
  cache: (float * 'filter, 'value list Lwt.t) NesCache.t ->
  values_getter: (unit -> 'value list Lwt.t) ->
  scoring_function: ('filter -> 'value -> float Lwt.t) ->
  tiebreakers: ('value -> 'value -> int Lwt.t) list ->
  ?pagination: Pagination.t ->
  ?threshold: float ->
  'filter ->
  (int * 'value list) Lwt.t
