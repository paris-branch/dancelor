(** {1 HTML} *)

open Js_of_ocaml

(** {2 Generic Types and Functions} *)

type dom_node = Dom.node Js.t
type document = Dom_html.document Js.t

type node

val node_of_dom_node : dom_node -> node
val node_to_dom_node : document -> node -> dom_node
val nodes_to_dom_nodes : document -> node list -> dom_node list

val append_nodes : dom_node -> document -> node list -> unit
val set_nodes : dom_node -> document -> node list -> unit

(** {2 Elements Builders} *)

(** {3 Children provider} *)

type 'a provider

type ('input, 'output) kind = 'input -> 'output provider
(** Label for the generic way to provide node children. *)

val const : ('a, 'a) kind
(** Label for constant children. *)

val lwt : ('a Lwt.t, 'a) kind
(** Label for children computed once from an Lwt promise. The node will still be
    returned immediately. Its children will be populated when the promise
    resolves. *)

val loop : (unit -> 'a Lwt.t, 'a) kind
(** Label for children computed on a loop from an Lwt promise. The node will
    still be returned immediately. Its children will be populated each time the
    promises resolve. The function in the loop should depend on something to
    make it not return instantly, for instance an {!Lwt_mvar} or an
    {!NesLwt_bchan}. *)

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

val a : ?href:string provider -> ?target:target -> ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Create an anchor element [<a></a>]. *)

val label : ?classes:string list -> ('node_list, node list) kind -> 'node_list-> node

type type_ = Checkbox | Text

val input : type_:type_ -> ?classes:string list -> ?placeholder:string -> ?on_input:(string -> unit) -> unit -> node

(** {3 Elements With No Children} *)

val hr : node
val br : node

val img : ?src:string provider -> ?classes:string list -> unit -> node
(** Create an image element [<img/>]. *)

val object_ : type_:string -> ?data:string provider -> ?classes:string list -> ('node_list, node list) kind -> 'node_list -> node
(** Create an object element [<object/>]. *)

val audio : ?src:string provider -> ?controls:bool -> ?classes:string list -> unit -> node
(** Create an audio element [<audio/>]. *)
