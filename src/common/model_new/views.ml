open Nes
open Ids
open Names
open Rows

module Person_view = struct
  type t = {
    id: Person_id.t;
    name: string;
    scddb_id: int option; [@default None]
    composed_tunes_are_public: bool; [@default false]
    published_tunes_are_public: bool; [@default false]
  }
  [@@deriving yojson, fields]
end

module Dance_view = struct
  type t = {
    id: Dance_id.t;
    name: string;
    extra_names: string list; [@default []]
    kind: Kind_dance.t;
    devisers: Person_name.t list; [@default []]
    scddb_id: int option; [@default None]
    disambiguation: string option; [@default None]
    date: PartialDate.t option; [@default None]
    two_chords: Model_builder.Core.Dance.two_chords;
    tunes: Tune_row.t list; [@default []]
  }
  [@@deriving yojson, fields]

  let to_name : t -> Dance_name.t = fun {id; name; _} ->
    {id; name}
end

module Source_view = struct
  type t = {
    id: Source_id.t;
    name: string;
    short_name: string option; [@default None]
    editors: Person_name.t list; [@default []]
    scddb_id: int option; [@default None]
    description: string option; [@default None]
    date: PartialDate.t option; [@default None]
  }
  [@@deriving yojson, fields]
end

module Tune_view = struct
  type version_row_without_tune = {
    id: Version_id.t;
    sources: Source_short_name.t list; [@default []]
    disambiguation: string option; [@default None]
    arrangers: Person_name.t list; [@default []]
    content: Version_row.content;
  }
  [@@deriving yojson, fields]

  type t = {
    id: Tune_id.t;
    name: string;
    kind: Kind_base.t;
    extra_names: string list; [@default []]
    composers: Person_name_with_details.t list; [@default []]
    dances: Dance_row.t list; [@default []]
    remark: string option; [@default None]
    scddb_id: int option; [@default None]
    date: PartialDate.t option; [@default None]
    versions: version_row_without_tune list; [@default []]
  }
  [@@deriving yojson, fields]

  let to_row : t -> Tune_row.t = fun {id; name; kind; composers; _} ->
    {id; name; kind; composers = List.map Person_name_with_details.to_name composers}

  let version_row_without_tune_to_version_row (tune : t) (version : version_row_without_tune) : Version_row.t = {
    id = version.id;
    tune = to_row tune;
    sources = version.sources;
    disambiguation = version.disambiguation;
    arrangers = version.arrangers;
    content = version.content;
  }
end

module Version_view = struct
  type content =
    | No_content
    | Destructured of {default_structure: Model_builder.Core.Version.Structure.t}
    | Monolithic of {bars: int; structure: Model_builder.Core.Version.Structure.t}
  [@@deriving yojson]

  type source = {
    id: Source_id.t;
    name: string;
    structure: Model_builder.Core.Version.Structure.t;
    details: string option; [@default None]
  }
  [@@deriving yojson, fields]

  let source_to_name : source -> Source_name.t = fun {id; name; _} ->
    {id; name}

  type t = {
    id: Version_id.t;
    tune: Tune_view.t;
    key: Music.Key.t;
    sources: source list; [@default []]
    arrangers: Person_name.t list; [@default []]
    remark: string option; [@default None]
    disambiguation: string option; [@default None]
    content: content;
  }
  [@@deriving yojson, fields]

  let to_name : t -> Version_name.t = fun {id; tune; _} ->
    {id; name = tune.name}
end

module Set_view = struct
  type t = {
    id: Set_id.t;
    name: string;
    conceptors: Person_name.t list; [@default []]
    kind: Kind.Dance.t;
    content: (Version_row.t * Model_builder.Core.Version_parameters.t) list; [@default []] (** FIXME: more compact content *)
    order: Model_builder.Core.Set_order.t;
    remark: string option; [@default None]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]

  let to_name : t -> Set_name.t = fun {id; name; _} ->
    {id; name}
end

module Book_view = struct
  type t = {
    id: Book_id.t;
    name: string;
    authors: Person_name.t list; [@default []]
    date: PartialDate.t option; [@default None]
    content: Model_builder.Core.Book.page list; (** FIXME: more compact pages*)
    remark: string option; [@default None]
    sources: Source_name.t list; [@default []]
    scddb_id: int option; [@default None]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]
end
