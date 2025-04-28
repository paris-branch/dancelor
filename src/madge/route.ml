open Nes
open Serialisation

include Route_internal

let return meth rt = Return (meth, rt)
let literal str route = Literal (str, route)
let variable ?(prefix = "") ?(suffix = "") rt route = Variable (prefix, rt, suffix, route)

let void () = return GET (module JVoid)
let get rt = return GET rt
let post rt = return POST rt
let head rt = return HEAD rt
let delete rt = return DELETE rt
let patch rt = return PATCH rt
let put rt = return PUT rt
let options rt = return OPTIONS rt
let trace rt = return TRACE rt
let connect rt = return CONNECT rt

let query_opt name rt route =
  let proxy = Option.some % (fun x f -> f x) in
  Query (Uri, name, proxy, Fun.id, rt, route)

let query name rt route =
  let proxy = Option.map (fun x f -> f x) in
  let unproxy = fun f x -> f (Some x) in
  Query (Uri, name, proxy, unproxy, rt, route)

let body_opt name rt route =
  let proxy = Option.some % (fun x f -> f x) in
  Query (Body, name, proxy, Fun.id, rt, route)

let body name rt route =
  let proxy = Option.map (fun x f -> f x) in
  let unproxy = fun f x -> f (Some x) in
  Query (Body, name, proxy, unproxy, rt, route)
