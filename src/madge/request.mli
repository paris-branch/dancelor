(** {1 Request}

    An HTTP request, according to Madge. *)

(** HTTP methods. *)
type meth = GET | POST | HEAD | DELETE | PATCH | PUT | OPTIONS | TRACE | CONNECT

val meth_to_string : meth -> string
val meth_to_cohttp_code_meth : meth -> Cohttp.Code.meth
val cohttp_code_meth_to_meth : Cohttp.Code.meth -> meth

(** Whether the method is safe, according to the HTTP specification. Safe
    methods are not supposed to have any side-effect on the server and can
    therefore be cached.
    See https://developer.mozilla.org/en-US/docs/Glossary/Safe/HTTP *)
val is_safe : meth -> bool

(** Abstract type of a request. *)
type t

(** Make a request from a method, a URI and a body. *)
val make : meth: meth -> uri: Uri.t -> body: string -> t

val meth : t -> meth
val uri : t -> Uri.t
val body : t -> string
