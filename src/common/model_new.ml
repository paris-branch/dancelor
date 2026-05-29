open Nes

(** Main suffixes:

   - “name”: just the id and the name, eg. for list of elements.
   - “row”: a little bit more, enough to make a table of those elements.
   - “view”: much more, enough to show a page about the element. *)

(** {2 Person} *)

module Person_name = struct
  type t = {
    id: Model_builder.Core.Person.t Entry.id;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Person_name_with_details = struct
  type t = {
    id: Model_builder.Core.Person.t Entry.id;
    name: string;
    details: string option; [@default None]
  }
  [@@deriving yojson, fields]
end

module Person_row = struct
  type t = Person_name.t = {
    id: Model_builder.Core.Person.t Entry.id;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Person_view = struct
  type t = {
    id: Model_builder.Core.Person.t Entry.id;
    name: string;
    scddb_id: int option; [@default None]
  }
  [@@deriving yojson, fields]
end

(** {2 Dance} *)

module Dance_name = struct
  type t = {
    id: Model_builder.Core.Dance.t Entry.id;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Dance_row = struct
  type t = {
    id: Model_builder.Core.Dance.t Entry.id;
    name: string;
    kind: Kind_dance.t;
    devisers: Person_name.t list; [@default []]
    disambiguation: string option; [@default None]
  }
  [@@deriving yojson, fields]
end

module Dance_view = struct
  type t = {
    id: Model_builder.Core.Dance.t Entry.id;
    name: string;
    extra_names: string list; [@default []]
    kind: Kind_dance.t;
    devisers: Person_name.t list; [@default []]
    scddb_id: int option; [@default None]
    disambiguation: string option; [@default None]
    date: PartialDate.t option; [@default None]
  }
  [@@deriving yojson, fields]
end

(** {2 Source} *)

module Source_name = struct
  type t = {
    id: Model_builder.Core.Source.t Entry.id;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Source_short_name = struct
  type t = {
    id: Model_builder.Core.Source.t Entry.id;
    short_name: string;
  }
  [@@deriving yojson, fields]
end

module Source_row = struct
  type t = {
    id: Model_builder.Core.Source.t Entry.id;
    name: string;
    date: PartialDate.t option; [@default None]
    editors: Person_name.t list; [@default []]
  }
  [@@deriving yojson, fields]
end

module Source_view = struct
  type t = {
    id: Model_builder.Core.Source.t Entry.id;
    name: string;
    short_name: string;
    editors: Person_name.t list; [@default []]
    scddb_id: int option; [@default None]
    description: string option; [@default None]
    date: PartialDate.t option; [@default None]
  }
  [@@deriving yojson, fields]
end

(** {2 Tune} *)

module Tune_name = struct
  type t = {
    id: Model_builder.Core.Tune.t Entry.id;
    name: string
  }
  [@@deriving yojson, fields]
end

module Tune_row = struct
  type t = {
    id: Model_builder.Core.Tune.t Entry.id;
    name: string;
    kind: Kind_base.t;
    composers: Person_name.t list; [@default []]
  }
  [@@deriving yojson, fields]
end

module Tune_view = struct
  type t = {
    id: Model_builder.Core.Tune.t Entry.id;
    name: string;
    extra_names: string list; [@default []]
    composers: Person_name.t list; [@default []]
    dances: Dance_row.t list; [@default []]
    remark: string option; [@default None]
    scddb_id: int option; [@default None]
    date: PartialDate.t option; [@default None]
  }
  [@@deriving yojson, fields]
end

(** {2 Version} *)

module Version_row = struct
  type content =
    | No_content
    | Destructured
    | Monolithic of int * Model_builder.Core.Version.Structure.t
  [@@deriving yojson]

  type t = {
    id: Model_builder.Core.Version.t Entry.id;
    tune: Tune_row.t;
    sources: Source_short_name.t list; [@default []]
    disambiguation: NEString.t option; [@default None]
    arrangers: Person_name.t list; [@default []]
    content: content;
  }
  [@@deriving yojson, fields]
end

module Version_view = struct
  type source = {
    id: Model_builder.Core.Source.t Entry.id;
    name: string;
    structure: Model_builder.Core.Version.Structure.t;
    details: NEString.t option; [@default None]
  }
  [@@deriving yojson, fields]

  type t = {
    id: Model_builder.Core.Version.t Entry.id;
    tune: Tune_view.t;
    key: Music.Key.t;
    sources: source list; [@default []]
    arrangers: Person_name.t list; [@default []]
    remark: NEString.t option; [@default None]
    disambiguation: NEString.t option; [@default None]
  }
  [@@deriving yojson, fields]
end

(** {2 Set} *)

module Set_row = struct
  type t = {
    id: Model_builder.Core.Set.t Entry.id;
    name: string;
    kind: Kind_dance.t;
    conceptors: Person_name.t list; [@default []]
    tunes: Tune_name.t list; [@default []]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]
end

module Set_view = struct
  type t = {
    id: Model_builder.Core.Set.t Entry.id;
    name: string;
    conceptors: Person_name.t list; [@default []]
    kind: Kind.Dance.t;
    contents: (Version_row.t * Model_builder.Core.Version_parameters.t) list; [@default []] (** FIXME: more compact content *)
    order: Model_builder.Core.Set_order.t;
    remark: string option; [@default None]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]
end

(** {2 Book} *)

module Book_row = struct
  type t = {
    id: Model_builder.Core.Book.t Entry.id;
    name: string;
    date: PartialDate.t option; [@default None]
    authors: Person_name.t list; [@default []]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]
end

module Book_view = struct
  type t = {
    name: string;
    authors: Person_name.t list; [@default []]
    date: PartialDate.t option; [@default None]
    contents: Model_builder.Core.Book.page list; (** FIXME: more compact pages*)
    remark: string option; [@default None]
    sources: Source_name.t list; [@default []]
    scddb_id: int option; [@default None]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]
end

(** {2 Any} *)

module Any_id = struct
  type t =
    | Person of Model_builder.Core.Person.t Entry.id
    | Dance of Model_builder.Core.Dance.t Entry.id
    | Source of Model_builder.Core.Source.t Entry.id
    | Tune of Model_builder.Core.Tune.t Entry.id
    | Version of (Model_builder.Core.Tune.t Entry.id * Model_builder.Core.Version.t Entry.id)
    | Set of Model_builder.Core.Set.t Entry.id
    | Book of Model_builder.Core.Book.t Entry.id
  [@@deriving yojson, variants]

  let equal any1 any2 =
    match any1, any2 with
    | Person id1, Person id2 -> Entry.Id.equal' id1 id2
    | Dance id1, Dance id2 -> Entry.Id.equal' id1 id2
    | Source id1, Source id2 -> Entry.Id.equal' id1 id2
    | Tune id1, Tune id2 -> Entry.Id.equal' id1 id2
    | Version (_, id1), Version (_, id2) -> Entry.Id.equal' id1 id2
    | Set id1, Set id2 -> Entry.Id.equal' id1 id2
    | Book id1, Book id2 -> Entry.Id.equal' id1 id2
    | _ -> false
end

module Any_row = struct
  type t =
    | Person of Person_row.t
    | Dance of Dance_row.t
    | Source of Source_row.t
    | Tune of Tune_row.t
    | Version of Version_row.t
    | Set of Set_row.t
    | Book of Book_row.t
  [@@deriving yojson, variants]

  let to_id : t -> Any_id.t = function
    | Person p -> Person p.id
    | Dance d -> Dance d.id
    | Source s -> Source s.id
    | Tune t -> Tune t.id
    | Version v -> Version (v.tune.id, v.id)
    | Set s -> Set s.id
    | Book b -> Book b.id
end
