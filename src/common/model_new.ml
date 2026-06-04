open Nes

(** Main suffixes:

   - “name”: just the id and the name, eg. for list of elements.
   - “row”: a little bit more, enough to make a table of those elements.
   - “view”: much more, enough to show a page about the element. *)

(** {2 Person} *)

module Person_id = struct
  type t = Model_builder.Core.Person.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Person_name = struct
  type t = {
    id: Person_id.t;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Person_name_with_details = struct
  type t = {
    id: Person_id.t;
    name: string;
    details: string option; [@default None]
  }
  [@@deriving yojson, fields]

  let to_name : t -> Person_name.t = fun {id; name; _} ->
    {id; name}
end

module Person_row = struct
  type t = Person_name.t = {
    id: Person_id.t;
    name: string;
  }
  [@@deriving yojson, fields]
end

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

(** {2 User} *)

module User_id = struct
  type t = Model_builder.Core.User.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

(** {2 Dance} *)

module Dance_id = struct
  type t = Model_builder.Core.Dance.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Dance_name = struct
  type t = {
    id: Dance_id.t;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Dance_row = struct
  type t = {
    id: Dance_id.t;
    name: string;
    kind: Kind_dance.t;
    devisers: Person_name.t list; [@default []]
    disambiguation: string option; [@default None]
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
  }
  [@@deriving yojson, fields]
end

(** {2 Source} *)

module Source_id = struct
  type t = Model_builder.Core.Source.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Source_name = struct
  type t = {
    id: Source_id.t;
    name: string;
  }
  [@@deriving yojson, fields]
end

module Source_short_name = struct
  type t = {
    id: Source_id.t;
    short_name: string;
  }
  [@@deriving yojson, fields]
end

module Source_row = struct
  type t = {
    id: Source_id.t;
    name: string;
    date: PartialDate.t option; [@default None]
    editors: Person_name.t list; [@default []]
  }
  [@@deriving yojson, fields]

  let to_name : t -> Source_name.t = fun {id; name; _} -> {id; name}
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

(** {2 Tune} *)

module Tune_id = struct
  type t = Model_builder.Core.Tune.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Tune_name = struct
  type t = {
    id: Tune_id.t;
    name: string
  }
  [@@deriving yojson, fields]
end

module Tune_row = struct
  type t = {
    id: Tune_id.t;
    name: string;
    kind: Kind_base.t;
    composers: Person_name.t list; [@default []]
  }
  [@@deriving yojson, fields]
end

module Tune_view = struct
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
  }
  [@@deriving yojson, fields]
end

(** {2 Version} *)

module Version_id = struct
  type t = Model_builder.Core.Version.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Version_name = struct
  type t = {
    id: Version_id.t;
    name: string
  }
  [@@deriving yojson, fields]
end

module Version_row = struct
  type content =
    | No_content
    | Destructured
    | Monolithic of {bars: int; structure: Model_builder.Core.Version.Structure.t}
  [@@deriving yojson]

  type t = {
    id: Version_id.t;
    tune: Tune_row.t;
    sources: Source_short_name.t list; [@default []]
    disambiguation: string option; [@default None]
    arrangers: Person_name.t list; [@default []]
    content: content;
  }
  [@@deriving yojson, fields]

  let to_name : t -> Version_name.t = fun {id; tune; _} ->
    {id; name = tune.name}
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

(** {2 Set} *)

module Set_id = struct
  type t = Model_builder.Core.Set.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Set_row = struct
  type t = {
    id: Set_id.t;
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
    id: Set_id.t;
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

module Book_id = struct
  type t = Model_builder.Core.Book.t Entry.id
  [@@deriving yojson]

  (* For URI serialisation *)
  let to_string = Entry.Id.to_string
  let of_string = Entry.Id.of_string
end

module Book_row = struct
  type t = {
    id: Book_id.t;
    name: string;
    date: PartialDate.t option; [@default None]
    authors: Person_name.t list; [@default []]
    permission: Permission_builder.can_get_private;
  }
  [@@deriving yojson, fields]
end

module Book_view = struct
  type t = {
    id: Book_id.t;
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
    | Person of Person_id.t
    | Dance of Dance_id.t
    | Source of Source_id.t
    | Tune of Tune_id.t
    | Version of Version_id.t
    | Set of Set_id.t
    | Book of Book_id.t
  [@@deriving yojson, variants]

  let equal any1 any2 =
    match any1, any2 with
    | Person id1, Person id2 -> Entry.Id.equal' id1 id2
    | Dance id1, Dance id2 -> Entry.Id.equal' id1 id2
    | Source id1, Source id2 -> Entry.Id.equal' id1 id2
    | Tune id1, Tune id2 -> Entry.Id.equal' id1 id2
    | Version id1, Version id2 -> Entry.Id.equal' id1 id2
    | Set id1, Set id2 -> Entry.Id.equal' id1 id2
    | Book id1, Book id2 -> Entry.Id.equal' id1 id2
    | _ -> false

  let to_entry_id = function
    | Person x -> Entry.Id.unsafe_coerce x
    | Dance x -> Entry.Id.unsafe_coerce x
    | Source x -> Entry.Id.unsafe_coerce x
    | Tune x -> Entry.Id.unsafe_coerce x
    | Version x -> Entry.Id.unsafe_coerce x
    | Set x -> Entry.Id.unsafe_coerce x
    | Book x -> Entry.Id.unsafe_coerce x
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
    | Version v -> Version v.id
    | Set s -> Set s.id
    | Book b -> Book b.id
end

(** {2 Other} *)

type 'a search_context_result = {
  total: int;
  previous_item: 'a option;
  index: int;
  next_item: 'a option;
}
[@@deriving yojson, fields]

type 'a search_result = {
  total: int;
  items: 'a list;
}
[@@deriving yojson, fields]
