open Nes

(** {2 Results} *)

module type Type = sig
  type t
  [@@deriving yojson]
end

module Search_result = struct
  type 'a t = {
    total: int;
    items: 'a list;
  }
  [@@deriving yojson, fields]

  let map f {total; items} = {total; items = List.map f items}
end

module Make_search_result (X : Type) = struct
  type t = X.t Search_result.t
  [@@deriving yojson]
end

module Search_context_result = struct
  type 'a t = {
    total: int;
    previous_item: 'a option;
    index: int;
    next_item: 'a option;
  }
  [@@deriving yojson, fields]
end

module Make_search_context_result (X : Type) = struct
  type t = X.t Search_context_result.t
  [@@deriving yojson]
end

(** {2 Queries} *)

module Query_string = Fresh.Make(String)

module Query = struct
  type common = {
    name: string;
  }
  [@@deriving yojson]

  type 'a t =
    {common: common; specific: 'a}
  [@@deriving yojson]
end

module Person_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]
end

module User_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]
end

module Dance_query = struct
  type specific = {
    kind: Kind_base.t list option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    kind = None;
  }

  type t = specific Query.t
  [@@deriving yojson]
end

module Source_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]
end

module Tune_query = struct
  type specific = {
    kind: Kind_base.t option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    kind = None;
  }

  type t = specific Query.t
  [@@deriving yojson]
end

module Version_query = struct
  type specific = {
    tune: Tune_query.specific;
    key: Music.Key.t option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    tune = Tune_query.no_specific;
    key = None;
  }

  type t = specific Query.t
  [@@deriving yojson]
end

module Set_query = struct
  type specific = {
    kind: Kind_base.t option; [@default None]
  }
  [@@deriving yojson]

  let no_specific = {
    kind = None;
  }

  type t = specific Query.t
  [@@deriving yojson]
end

module Book_query = struct
  type specific = unit
  [@@deriving yojson]

  let no_specific = ()

  type t = specific Query.t
  [@@deriving yojson]
end

module Any_query = struct
  type model_specific =
    | Person of Person_query.specific
    | User of User_query.specific
    | Dance of Dance_query.specific
    | Source of Source_query.specific
    | Tune of Tune_query.specific
    | Version of Version_query.specific
    | Set of Set_query.specific
    | Book of Book_query.specific
  [@@deriving yojson]

  type specific = model_specific option
  [@@deriving yojson]

  type t = specific Query.t
  [@@deriving yojson]
end
