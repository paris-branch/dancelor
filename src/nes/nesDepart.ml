let next_key = ref 0
let gc_roots = ref []

let keep value =
  let key = !next_key in
  incr next_key;
  gc_roots := (key, Obj.magic value) :: !gc_roots;
  key

let free key =
  gc_roots := List.remove_assq key !gc_roots

let depends ~on value =
  let key = keep value in
  Gc.finalise_last (fun () -> free key) on

let keep_forever value =
  ignore (keep value)
