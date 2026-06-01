open Model_new

module type Type = sig
  type t
  [@@deriving yojson]
end

module Search_result (X : Type) = struct
  type t = X.t search_result
  [@@deriving yojson]
end

module Search_context_result (X : Type) = struct
  type t = X.t search_context_result
  [@@deriving yojson]
end
