open Nes

type header = Cohttp.Header.t
type header_proxy = (string * string) list [@@deriving biniou, yojson]
let header_to_biniou = header_proxy_to_biniou % Cohttp.Header.to_list
let header_of_biniou_exn = Cohttp.Header.of_list % header_proxy_of_biniou_exn
let header_to_yojson = header_proxy_to_yojson % Cohttp.Header.to_list
let header_of_yojson = Result.map Cohttp.Header.of_list % header_proxy_of_yojson

type version = [%import: Cohttp.Code.version] [@@deriving biniou, yojson]

type status_code = [%import: Cohttp.Code.status_code] [@@deriving biniou, yojson]

type encoding = [%import: Cohttp.Transfer.encoding] [@@deriving biniou, yojson]

type response = [%import: Cohttp.Response.t [@with Cohttp__.Header.t := header;
  Cohttp__.Code.version := version;
  Cohttp__.Code.status_code := status_code;
  Cohttp__.Transfer.encoding := encoding;
  ]
]
[@@deriving biniou, yojson]

type body = [%import: Cohttp.Body.t] [@@deriving biniou, yojson]

type t = response * body [@@deriving biniou, yojson]

let body_of_lwt = Lwt.map Cohttp.Body.of_string_list % Cohttp_lwt.Body.to_string_list
