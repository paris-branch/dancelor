open Nes
open Html

let prepare ?(label = "Nil") () : (unit, unit) Component.s = (module struct
  let label = label
  type value = unit
  type state = unit [@@deriving yojson]
  let value_to_state () = lwt_unit
  let empty = ()
  let from_initial_text _ = ()
  type t = Nil
  let initialise () = lwt Nil
  let signal Nil = S.const (Ok ())
  let state Nil = S.const ()
  let focus Nil = ()
  let set Nil () = lwt_unit
  let trigger Nil = ()
  let clear Nil = ()
  let inner_html Nil = div []
  let actions Nil = S.const []
end)
