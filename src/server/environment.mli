type session

val user : session -> unit option

type t

val session : t -> session

val make : request: Cohttp.Request.t -> unit -> t

val add_session_cookie : t -> Cohttp.Header.t -> Cohttp.Header.t
