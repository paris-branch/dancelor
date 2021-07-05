open Js_of_ocaml

type dom_node = Dom.node Js.t
type document = Dom_html.document Js.t

type node

val node_of_dom_node : dom_node -> node
val append_nodes : dom_node -> document -> node list -> unit

val text_lwt : string Lwt.t -> node
val text : string -> node

type node_maker_lwt = ?classes:string list -> node list Lwt.t -> node
type node_maker = ?classes:string list -> node list -> node

val h1_lwt : node_maker_lwt
val h1 : node_maker

val h2_lwt : node_maker_lwt
val h2 : node_maker

val h3_lwt : node_maker_lwt
val h3 : node_maker

val h4_lwt : node_maker_lwt
val h4 : node_maker

val h5_lwt : node_maker_lwt
val h5 : node_maker

val h6_lwt : node_maker_lwt
val h6 : node_maker

val p_lwt : node_maker_lwt
val p : node_maker

val div_lwt : node_maker_lwt
val div : node_maker

val a_lwt : ?href:string -> ?href_lwt:string Lwt.t -> node_maker_lwt
val a     : ?href:string -> ?href_lwt:string Lwt.t -> node_maker
(** Create an anchor element [<a>].

    @raises Invalid_argument if none or both [?href] and [?href_lwt] are set. *)

val hr : node
val br : node
