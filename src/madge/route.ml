open Nes
open Serialisation

include Route_internal

let return meth serialiser = Return {meth; serialiser}
let literal str rest = Literal {str; rest}
let variable ?(prefix = "") ?(suffix = "") serialiser rest = Variable {prefix; serialiser; suffix; rest}

let void () = return GET (module JVoid)
let get serialiser = return GET serialiser
let post serialiser = return POST serialiser
let head serialiser = return HEAD serialiser
let delete serialiser = return DELETE serialiser
let patch serialiser = return PATCH serialiser
let put serialiser = return PUT serialiser
let options serialiser = return OPTIONS serialiser
let trace serialiser = return TRACE serialiser
let connect serialiser = return CONNECT serialiser

let query_or_body kind name rest =
  let proxy = function `Present x -> `Match x | `Absent -> `Dont_match in
  let unproxy y = `Present y in
  Query_or_body {kind; name; proxy; unproxy; rest}

let query_or_body_opt kind name rest =
  let proxy = function `Present x -> `Match (Some x) | `Absent -> `Match None in
  let unproxy = Option.fold ~none: `Absent ~some: (fun y -> `Present y) in
  Query_or_body {kind; name; proxy; unproxy; rest}

let query_or_body_def kind name ?(eq = Stdlib.(=)) ~def rest =
  let proxy = function `Present x -> `Match x | `Absent -> `Match def in
  let unproxy y = if eq y def then `Absent else `Present y in
  Query_or_body {kind; name; proxy; unproxy; rest}

let query_json name serialiser rest = query_or_body (`Query_json serialiser) name rest
let query_json_opt name serialiser rest = query_or_body_opt (`Query_json serialiser) name rest
let query_json_def name serialiser ?eq ~def rest = query_or_body_def (`Query_json serialiser) name ?eq ~def rest

let query_str name serialiser rest = query_or_body (`Query_string serialiser) name rest
let query_str_opt name serialiser rest = query_or_body_opt (`Query_string serialiser) name rest
let query_str_def name serialiser ?eq ~def rest = query_or_body_def (`Query_string serialiser) name ?eq ~def rest

let body name serialiser rest = query_or_body (`Body serialiser) name rest
let body_opt name serialiser rest = query_or_body_opt (`Body serialiser) name rest
let body_def name serialiser ?eq ~def rest = query_or_body_def (`Body serialiser) name ?eq ~def rest
