type serialised = Yojson.Safe.t
type 'a serialiser = 'a -> serialised
type 'a unserialiser = serialised -> ('a, string) result

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
