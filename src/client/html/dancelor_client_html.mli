(** {1 HTML} *)

open Js_of_ocaml

(** {2 Generic Types and Functions} *)

type dom_node = Dom.node Js.t
type document = Dom_html.document Js.t

type node

val node_of_dom_node : dom_node -> node
val node_to_dom_node : document -> node -> dom_node
val nodes_to_dom_nodes : document -> node list -> dom_node list

val append_node : dom_node -> document -> node -> unit
val append_nodes : dom_node -> document -> node list -> unit

(** {2 Elements Builders} *)

(** {3 Raw Text} *)

val text_lwt : string Lwt.t -> node
val text : string -> node

(** {3 Elements With Children} *)

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

val span_lwt : node_maker_lwt
val span : node_maker

val a_lwt : ?href:string -> ?href_lwt:string Lwt.t -> node_maker_lwt
val a     : ?href:string -> ?href_lwt:string Lwt.t -> node_maker
(** Create an anchor element [<a>].

    @raises Invalid_argument if none or both [?href] and [?href_lwt] are set. *)

(** {3 Elements With No Children} *)

val hr : node
val br : node

val img :
  ?src:string -> ?src_lwt:string Lwt.t ->
  ?classes:string list -> unit ->
  node
