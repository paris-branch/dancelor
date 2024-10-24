include Lwt

let compose f g = fun x ->
  bind (f x) g

(** Poor man's sleep: same as {!Lwt_unix.sleep} except that it is less efficient
    but it does not depend on [lwt.unix], which matters when compiling to eg.
    JavaScript. *)
let pmsleep duration =
  let target = Unix.gettimeofday () +. duration in
  let rec loop () =
    Lwt.pause ();%lwt
    if Unix.gettimeofday () >= target then
      Lwt.return_unit
    else
      loop ()
  in
  loop ()

module Syntax = struct
  let (>>=|) = bind
  let (>=>|) = compose
  let (>|=|) p f = map f p
end
