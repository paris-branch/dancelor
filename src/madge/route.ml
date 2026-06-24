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

let query name serialiser rest =
  let proxy = function `Present x -> `Match x | `Absent -> `Dont_match in
  let unproxy y = `Present y in
  Query_or_body {kind = `Query; name; proxy; unproxy; serialiser; rest}

let query_opt name serialiser rest =
  let proxy = function `Present x -> `Match (Some x) | `Absent -> `Match None in
  let unproxy = Option.fold ~none: `Absent ~some: (fun y -> `Present y) in
  Query_or_body {kind = `Query; name; proxy; unproxy; serialiser; rest}

let query_def name serialiser ?(eq = Stdlib.(=)) default rest =
  let proxy = function `Present x -> `Match x | `Absent -> `Match default in
  let unproxy y = if eq y default then `Absent else `Present y in
  Query_or_body {kind = `Query; name; proxy; unproxy; serialiser; rest}

let body name serialiser rest =
  let proxy = function `Present x -> `Match x | `Absent -> `Dont_match in
  let unproxy y = `Present y in
  Query_or_body {kind = `Body; name; proxy; unproxy; serialiser; rest}

let body_opt name serialiser rest =
  let proxy = function `Present x -> `Match (Some x) | `Absent -> `Match None in
  let unproxy = Option.fold ~none: `Absent ~some: (fun y -> `Present y) in
  Query_or_body {kind = `Body; name; proxy; unproxy; serialiser; rest}

let body_def name serialiser ?(eq = Stdlib.(=)) default rest =
  let proxy = function `Present x -> `Match x | `Absent -> `Match default in
  let unproxy y = if eq y default then `Absent else `Present y in
  Query_or_body {kind = `Body; name; proxy; unproxy; serialiser; rest}
