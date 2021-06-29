(** {1 Formula} *)

type 'filter t =
  | False
  | True
  | Not of 'filter t
  | And of 'filter t * 'filter t
  | Or  of 'filter t * 'filter t
  | Pred of 'filter
[@@deriving yojson]

let false_ = False
let true_ = True

let not_ f = Not f

let and_ f1 f2 = And(f1, f2)
let and_l = function
  | [] -> True
  | h::t -> List.fold_left and_ h t

let or_ f1 f2 = Or(f1, f2)
let or_l = function
  | [] -> False
  | h::t -> List.fold_left or_ h t

let pred value = Pred value

let accepts faccepts form value =
  let rec accepts = function
    | False -> Lwt.return_false
    | True -> Lwt.return_true
    | Not f ->
      let%lwt a = accepts f in
      Lwt.return (not a)
    | And (f1, f2) ->
      let%lwt a1 = accepts f1
      and     a2 = accepts f2 in
      Lwt.return (a1 && a2)
    | Or (f1, f2) ->
      let%lwt a1 = accepts f1
      and     a2 = accepts f2 in
      Lwt.return (a1 || a2)
    | Pred p ->
      faccepts p value
  in
  accepts form

(** {2 Serialisable} *)

module Make_Serialisable (M : Madge_common.SERIALISABLE) = struct
  type nonrec t = M.t t

  let _key = M._key ^ "-filter"

  let of_yojson = of_yojson M.of_yojson
  let to_yojson = to_yojson M.to_yojson
end
