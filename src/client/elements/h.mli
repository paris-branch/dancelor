open Js_of_ocaml

type dom_node = Dom.node Js.t
type document = Dom_html.document Js.t

type node

val node_of_dom_node : dom_node -> node
val append_nodes : dom_node -> document -> node list -> unit

val text_lwt : string Lwt.t -> node
val text : string -> node

val h1_lwt : ?classes:string list -> node list Lwt.t -> node
val h1 : ?classes:string list -> node list -> node

val h2_lwt : ?classes:string list -> node list Lwt.t -> node
val h2 : ?classes:string list -> node list -> node

val h3_lwt : ?classes:string list -> node list Lwt.t -> node
val h3 : ?classes:string list -> node list -> node

val h4_lwt : ?classes:string list -> node list Lwt.t -> node
val h4 : ?classes:string list -> node list -> node

val h5_lwt : ?classes:string list -> node list Lwt.t -> node
val h5 : ?classes:string list -> node list -> node

val h6_lwt : ?classes:string list -> node list Lwt.t -> node
val h6 : ?classes:string list -> node list -> node

val p_lwt : ?classes:string list -> node list Lwt.t -> node
val p : ?classes:string list -> node list -> node

val div_lwt : ?classes:string list -> node list Lwt.t -> node
val div : ?classes:string list -> node list -> node

val a_lwt : ?href:string -> ?href_lwt:string Lwt.t -> ?classes:string list -> node list Lwt.t -> node
val a     : ?href:string -> ?href_lwt:string Lwt.t -> ?classes:string list -> node list -> node
(** Create an anchor element [<a>].

    @raises Invalid_argument if none or both [?href] and [?href_lwt] are set. *)
