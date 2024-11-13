let () =
  Ppx_monad.register
    "rlwt"
    ~monad: "NesRlwt"

let () =
  Ppx_monad.register
    "elwt"
    ~monad_error: "NesRlwt"
