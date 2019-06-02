type serialised = Yojson.Safe.t
type 'a serialiser = 'a -> serialised
type 'a unserialiser = serialised -> ('a, string) result

type 'a arg =
  { opt: bool ;
    key : string ;
    serialiser : 'a serialiser ;
    unserialiser : 'a unserialiser }

(* FIXME: check either through typing or dynamically that optional arguments
   are indeed used optionally. *)

let arg ~key ~serialiser ~unserialiser =
  { opt = false; key; serialiser; unserialiser }

let optarg ~key ~serialiser ~unserialiser =
  { opt = true; key; serialiser; unserialiser }

let arg_key arg = arg.key
let arg_serialiser arg = arg.serialiser
let arg_unserialiser arg = arg.unserialiser

type 'a endpoint =
  { meth : Cohttp.Code.meth ;
    path : string ;
    serialiser : 'a serialiser ;
    unserialiser : 'a unserialiser }

let endpoint ~meth ~path ~serialiser ~unserialiser =
  { meth; path; serialiser; unserialiser }

let endpoint_meth endpoint = endpoint.meth
let endpoint_path endpoint = endpoint.path
let endpoint_serialiser endpoint = endpoint.serialiser
let endpoint_unserialiser endpoint = endpoint.unserialiser

type query = (string * string list) list ref

exception BadQuery of string
let bad_query string = raise (BadQuery string)

type my_unit = unit
[@@deriving yojson]
let unit_to_yojson = my_unit_to_yojson
let unit_of_yojson = my_unit_of_yojson

type my_float = float
[@@deriving yojson]
let float_to_yojson = my_float_to_yojson
let float_of_yojson = my_float_of_yojson

type my_string = string
[@@deriving yojson]
let string_to_yojson = my_string_to_yojson
let string_of_yojson = my_string_of_yojson

type 'a my_option = 'a option
[@@deriving yojson]
let option_to_yojson = my_option_to_yojson
let option_of_yojson = my_option_of_yojson

type 'a my_list = 'a list
[@@deriving yojson]
let list_to_yojson = my_list_to_yojson
let list_of_yojson = my_list_of_yojson

let prefix = ref "/madge"
