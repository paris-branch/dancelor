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

type 'children node_maker = ?classes:string list -> 'children -> node

(** {3 Raw Text} *)

val text_lwt : string Lwt.t -> node
val text : string -> node

(** {3 Elements With Children} *)

type std_node_maker_lwt = node list Lwt.t node_maker
type std_node_maker     = node list       node_maker

val h1_lwt : std_node_maker_lwt
val h1     : std_node_maker

val h2_lwt : std_node_maker_lwt
val h2     : std_node_maker

val h3_lwt : std_node_maker_lwt
val h3     : std_node_maker

val h4_lwt : std_node_maker_lwt
val h4     : std_node_maker

val h5_lwt : std_node_maker_lwt
val h5     : std_node_maker

val h6_lwt : std_node_maker_lwt
val h6     : std_node_maker

val p_lwt : std_node_maker_lwt
val p     : std_node_maker

val div_lwt : std_node_maker_lwt
val div     : std_node_maker

val span_lwt : std_node_maker_lwt
val span     : std_node_maker

val a_lwt : ?href:string -> ?href_lwt:string Lwt.t -> std_node_maker_lwt
val a     : ?href:string -> ?href_lwt:string Lwt.t -> std_node_maker
(** Create an anchor element [<a></a>].

    @raises Invalid_argument if none or both [?href] and [?href_lwt] are set. *)

val table_lwt : std_node_maker_lwt
val table     : std_node_maker

val thead_lwt : std_node_maker_lwt
val thead     : std_node_maker

val tbody_lwt : std_node_maker_lwt
val tbody     : std_node_maker

val tfoot_lwt : std_node_maker_lwt
val tfoot     : std_node_maker

val tr_lwt : std_node_maker_lwt
val tr     : std_node_maker

val td_lwt : ?colspan:int -> ?rowspan:int -> std_node_maker_lwt
val td     : ?colspan:int -> ?rowspan:int -> std_node_maker

val th_lwt : ?colspan:int -> ?rowspan:int -> std_node_maker_lwt
val th     : ?colspan:int -> ?rowspan:int -> std_node_maker

(** {3 Elements With No Children} *)

val hr : node
val br : node

val img : ?src:string -> ?src_lwt:string Lwt.t -> unit node_maker
(** Create an image element [<img/>].

    @raises Invalid_argument if none or both [?src] and [?src_lwt] are set. *)
