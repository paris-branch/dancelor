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

val text     : string       -> node
val text_lwt : string Lwt.t -> node

(** {3 Elements With Children} *)

val h1 : ?classes:string list -> node list -> node
(** Create a first-level title element [<h1></h1>]. *)

val h1_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} except it receives children as a delayed list. *)

val h2     : ?classes:string list -> node list -> node
val h2_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for second-level title elements [<h2></h2>]. *)

val h3     : ?classes:string list -> node list -> node
val h3_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for third-level title elements [<h3></h3>]. *)

val h4     : ?classes:string list -> node list -> node
val h4_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for fourth-level title elements [<h4></h4>]. *)

val h5     : ?classes:string list -> node list -> node
val h5_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for fifth-level title elements [<h5></h5>]. *)

val h6     : ?classes:string list -> node list -> node
val h6_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for sixth-level title elements [<h6></h6>]. *)

val p     : ?classes:string list -> node list -> node
val p_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for paragraph elements [<p></p>]. *)

val div     : ?classes:string list -> node list -> node
val div_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for div elements [<div></div>]. *)

val span     : ?classes:string list -> node list -> node
val span_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for span elements [<span></span>]. *)

val table     : ?classes:string list -> node list -> node
val table_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table elements [<table></table>]. *)

val thead     : ?classes:string list -> node list -> node
val thead_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table header elements [<thead></thead>]. *)

val tbody     : ?classes:string list -> node list -> node
val tbody_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table body elements [<tbody></tbody>]. *)

val tfoot     : ?classes:string list -> node list -> node
val tfoot_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table footer elements [<tfoot></tfoot>]. *)

val tr     : ?classes:string list -> node list -> node
val tr_lwt : ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table row elements [<tr></tr>]. *)

val td     : ?colspan:int -> ?rowspan:int -> ?classes:string list -> node list -> node
val td_lwt : ?colspan:int -> ?rowspan:int -> ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table cell elements [<td></td>]. *)

val th     : ?colspan:int -> ?rowspan:int -> ?classes:string list -> node list -> node
val th_lwt : ?colspan:int -> ?rowspan:int -> ?classes:string list -> node list Lwt.t -> node
(** Same as {!h1} and {!h1_lwt} for table header cell elements [<th></th>]. *)

val ol : ?classes:string list -> node list -> node
val ol_lwt : ?classes:string list -> node list Lwt.t -> node

val ul : ?classes:string list -> node list -> node
val ul_lwt : ?classes:string list -> node list Lwt.t -> node

val li : ?classes:string list -> node list -> node
val li_lwt : ?classes:string list -> node list Lwt.t -> node

val i : ?classes:string list -> node list -> node
val i_lwt : ?classes:string list -> node list Lwt.t -> node

type target = Blank | Self | Parent | Top | Frame of string

val a     : ?href:string -> ?href_lwt:string Lwt.t -> ?target:target -> ?classes:string list -> node list -> node
val a_lwt : ?href:string -> ?href_lwt:string Lwt.t -> ?target:target -> ?classes:string list -> node list Lwt.t -> node
(** Create an anchor element [<a></a>].

    @raise Invalid_argument if none or both [?href] and [?href_lwt] are set. *)

val label : ?classes:string list -> node list -> node
val label_lwt : ?classes:string list -> node list Lwt.t -> node

type type_ = Checkbox

val input : type_:type_ -> ?classes:string list -> unit -> node

(** {3 Elements With No Children} *)

val hr : node
val br : node

val img : ?src:string -> ?src_lwt:string Lwt.t -> ?classes:string list -> unit -> node
(** Create an image element [<img/>].

    @raise Invalid_argument if none or both [?src] and [?src_lwt] are set. *)

val object_ : type_:string -> ?data:string -> ?data_lwt:string Lwt.t -> ?classes:string list -> node list -> node
val object_lwt : type_:string -> ?data:string -> ?data_lwt:string Lwt.t -> ?classes:string list -> node list Lwt.t -> node
(** Create an object element [<object/>].

    @raise Invalid_argument if none or both [?data] and [?data_lwt] are set. *)

val audio : ?src:string -> ?src_lwt:string Lwt.t -> ?controls:bool -> ?classes:string list -> unit -> node
(** Create an audio element [<audio/>].

    @raise Invalid_argument if none or both [?src] and [?src_lwt] are set. *)
