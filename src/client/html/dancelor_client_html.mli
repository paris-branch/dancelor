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

(** {3 Raw Text}

    Raw text is not {i per se} an HTML element; it does not support attributes.
    It is the only element that actually shows text. *)

val text : string -> node
val text_lwt : string Lwt.t -> node

(** {3 Node Maker Types}

    There are several kinds of makers sharing a very similar interface. The only
    difference is that they receive their children in different ways, mainly as a
    list or as a delayed list. *)

type 'children node_maker = ?classes: string list -> 'children -> node
(** Common type for all node maker elements (except {!text} above). *)

type std_node_maker = node list node_maker
(** Type for node makers that directly receive their children as a list. *)

type std_node_maker_lwt = node list Lwt.t node_maker
(** Type for node makers that receive their children as a delayed list. *)

(** {3 Elements With Children} *)

val h1 : std_node_maker
(** Create a first-level title element [<h1></h1>]. *)

val h1_lwt : std_node_maker_lwt
(** Same as {!h1} except it receives children as a delayed list. *)

val h2 : std_node_maker
val h2_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for second-level title elements [<h2></h2>]. *)

val h3 : std_node_maker
val h3_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for third-level title elements [<h3></h3>]. *)

val h4 : std_node_maker
val h4_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for fourth-level title elements [<h4></h4>]. *)

val h5 : std_node_maker
val h5_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for fifth-level title elements [<h5></h5>]. *)

val h6 : std_node_maker
val h6_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for sixth-level title elements [<h6></h6>]. *)

val p : std_node_maker
val p_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for paragraph elements [<p></p>]. *)

val div : std_node_maker
val div_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for div elements [<div></div>]. *)

val span : std_node_maker
val span_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for span elements [<span></span>]. *)

val table : std_node_maker
val table_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table elements [<table></table>]. *)

val thead : std_node_maker
val thead_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table header elements [<thead></thead>]. *)

val tbody : std_node_maker
val tbody_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table body elements [<tbody></tbody>]. *)

val tfoot : std_node_maker
val tfoot_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table footer elements [<tfoot></tfoot>]. *)

val tr : std_node_maker
val tr_lwt : std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table row elements [<tr></tr>]. *)

val td : ?colspan: int -> ?rowspan: int -> std_node_maker
val td_lwt : ?colspan: int -> ?rowspan: int -> std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table cell elements [<td></td>]. *)

val th : ?colspan: int -> ?rowspan: int -> std_node_maker
val th_lwt : ?colspan: int -> ?rowspan: int -> std_node_maker_lwt
(** Same as {!h1} and {!h1_lwt} for table header cell elements [<th></th>]. *)

val ol : std_node_maker
val ol_lwt : std_node_maker_lwt

val ul : std_node_maker
val ul_lwt : std_node_maker_lwt

val li : std_node_maker
val li_lwt : std_node_maker_lwt

val i : std_node_maker
val i_lwt : std_node_maker_lwt

type target = Blank | Self | Parent | Top | Frame of string

val a : ?href: string -> ?href_lwt: string Lwt.t -> ?target: target -> std_node_maker
val a_lwt : ?href: string -> ?href_lwt: string Lwt.t -> ?target: target -> std_node_maker_lwt
(** Create an anchor element [<a></a>].

    @raise Invalid_argument if none or both [?href] and [?href_lwt] are set. *)

val label : std_node_maker
val label_lwt : std_node_maker_lwt

type type_ = Checkbox

val input : type_: type_ -> unit node_maker

(** {3 Elements With No Children} *)

val hr : node
val br : node

val img : ?src: string -> ?src_lwt: string Lwt.t -> unit node_maker
(** Create an image element [<img/>].

    @raise Invalid_argument if none or both [?src] and [?src_lwt] are set. *)

val object_ : type_: string -> ?data: string -> ?data_lwt: string Lwt.t -> std_node_maker
val object_lwt : type_: string -> ?data: string -> ?data_lwt: string Lwt.t -> std_node_maker_lwt
(** Create an object element [<object/>].

    @raise Invalid_argument if none or both [?data] and [?data_lwt] are set. *)

val audio : ?src: string -> ?src_lwt: string Lwt.t -> ?controls: bool -> unit node_maker
(** Create an audio element [<audio/>].

    @raise Invalid_argument if none or both [?src] and [?src_lwt] are set. *)
