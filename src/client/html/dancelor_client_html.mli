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

(** {3 Children provider} *)

type ('input, 'output) kind
(** Label for the generic way to provide node children. *)

val const : ('a, 'a) kind
(** Label for constant children. *)

val lwt : ('a Lwt.t, 'a) kind
(** Label for children computed once from an Lwt promise. The node will still be
    returned immediately. Its children will be populated when the promises
    resolves. *)

(* FIXME: Add a label for loops. *)

(** {3 Raw Text}

    Raw text is not {i per se} an HTML element; it does not support attributes.
    It is the only element that actually shows text. *)

val text : ('string, string) kind -> 'string -> node

(** {3 Elements With Children} *)

val h1 : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Create a first-level title element [<h1></h1>]. *)

val h2 : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} and for second-level title elements [<h2></h2>]. *)

val h3 : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} and for third-level title elements [<h3></h3>]. *)

val h4 : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for fourth-level title elements [<h4></h4>]. *)

val h5 : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for fifth-level title elements [<h5></h5>]. *)

val h6 : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for sixth-level title elements [<h6></h6>]. *)

val p : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for paragraph elements [<p></p>]. *)

val div : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for div elements [<div></div>]. *)

val span : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for span elements [<span></span>]. *)

val table : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table elements [<table></table>]. *)

val thead : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table header elements [<thead></thead>]. *)

val tbody : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table body elements [<tbody></tbody>]. *)

val tfoot : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table footer elements [<tfoot></tfoot>]. *)

val tr : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table row elements [<tr></tr>]. *)

val td : ?colspan:int -> ?rowspan:int -> ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table cell elements [<td></td>]. *)

val th : ?colspan:int -> ?rowspan:int -> ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Same as {!h1} for table header cell elements [<th></th>]. *)

val ol : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
val ul : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
val li : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node

val i : ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node

type target = Blank | Self | Parent | Top | Frame of string

val a : ?href:string -> ?href_lwt:string Lwt.t -> ?target:target -> ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Create an anchor element [<a></a>].

    @raise Invalid_argument if none or both [?href] and [?href_lwt] are set. *)

val label : ?classes:string list -> ('node_list, node list) kind -> 'node_list-> node

type type_ = Checkbox

val input : type_:type_ -> ?classes:string list -> unit -> node

(** {3 Elements With No Children} *)

val hr : node
val br : node

val img : ?src:string -> ?src_lwt:string Lwt.t -> ?classes:string list -> unit -> node
(** Create an image element [<img/>].

    @raise Invalid_argument if none or both [?src] and [?src_lwt] are set. *)

val object_ : type_:string -> ?data:string -> ?data_lwt:string Lwt.t -> ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Create an object element [<object/>].

    @raise Invalid_argument if none or both [?data] and [?data_lwt] are set. *)

val audio : ?src:string -> ?src_lwt:string Lwt.t -> ?controls:bool -> ?classes:string list -> unit -> node
(** Create an audio element [<audio/>].

    @raise Invalid_argument if none or both [?src] and [?src_lwt] are set. *)
