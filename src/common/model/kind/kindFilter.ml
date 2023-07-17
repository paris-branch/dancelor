open Nes

(* Dance *)

module Dance = struct
  type predicate =
    | Is of KindDance.t
    | Simple
    | Version of KindVersion.Filter.t
  [@@deriving yojson]

  type t = predicate Formula.t
  [@@deriving yojson]

  let is kind = Formula.pred (Is kind)
  let version vfilter = Formula.pred (Version vfilter)
  let base bfilter = version (KindVersion.Filter.base bfilter)

  let accepts filter kind =
    Formula.interpret filter @@ function

    | Is kind' ->
      Lwt.return (Formula.interpret_bool (kind = kind'))

    | Simple ->
      (match kind with
       | _, [_] -> Lwt.return Formula.interpret_true
       | _ -> Lwt.return Formula.interpret_false)

    | Version vfilter ->
      (match kind with
       | _, [vkind] -> KindVersion.Filter.accepts vfilter vkind
       | _ -> Lwt.return Formula.interpret_false)

  let raw string =
    match KindBase.of_string_opt string with
    | Some bkind -> Ok (base (KindBase.Filter.is bkind))
    | None ->
      match KindVersion.of_string_opt string with
      | Some vkind -> Ok (version (KindVersion.Filter.is vkind))
      | None ->
        match KindDance.of_string_opt string with
        | Some dkind -> Ok (is dkind)
        | None -> error_fmt "could not interpret \"%s\" as a kind for dances" string

  let nullary_text_predicates = []

  (* Unary text_predicates lifted from Versions. *)
  let unary_text_predicates =
    List.map
      (fun (name, builder) ->
         (name,
          (fun formula ->
             match builder formula with
             | Ok formula -> Ok (version formula)
             | Error err -> Error err)))
      KindVersion.Filter.unary_text_predicates

  let from_text_formula =
    TextFormula.make_to_formula raw
      nullary_text_predicates
      unary_text_predicates
end
