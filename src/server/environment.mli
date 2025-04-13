type t

val make : request: Cohttp.Request.t -> unit -> t

val add_session_cookie : t -> Cohttp.Header.t -> Cohttp.Header.t
