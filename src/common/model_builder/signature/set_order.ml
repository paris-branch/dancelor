module type S = sig
  type component = Core.Set_order.component =
    | External of int
    | Internal of int

  type t = component list

  val of_string_opt : string -> t option
  val to_string : t -> string
  val to_pretty_string : t -> string
end
