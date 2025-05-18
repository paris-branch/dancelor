open Nes

type book_predicate =
  | Is of ModelBuilder.Core.Book.t Slug.t
  | IsSource
  | Title of string
  | TitleMatches of string
  | Subtitle of string
  | SubtitleMatches of string
  | ExistsVersion of version
  | ExistsSet of set
  | ExistsInlineSet of set
  | ExistsVersionDeep of version

and book = book_predicate Formula.t

and dance_predicate =
  | Is of ModelBuilder.Core.Dance.t Slug.t
  | Name of string
  | NameMatches of string
  | Kind of Kind.Dance.Filter.t
  | ExistsDeviser of person

and dance = dance_predicate Formula.t

and person_predicate =
  | Is of ModelBuilder.Core.Person.t Slug.t
  | Name of string
  | NameMatches of string

and person = person_predicate Formula.t

and set_predicate =
  | Is of ModelBuilder.Core.Set.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsConceptor of person
  | ExistsVersion of version
  | Kind of Kind.Dance.Filter.t

and set = set_predicate Formula.t

and source_predicate =
  | Is of ModelBuilder.Core.Source.t Slug.t
  | Name of string
  | NameMatches of string

and source = source_predicate Formula.t

and tune_predicate =
  | Is of ModelBuilder.Core.Tune.t Slug.t
  | Name of string
  | NameMatches of string
  | ExistsComposer of person
  | Kind of Kind.Base.Filter.t
  | ExistsDance of dance

and tune = tune_predicate Formula.t

and version_predicate =
  | Is of ModelBuilder.Core.Version.t Slug.t
  | Tune of tune
  | Key of Music.key
  | Kind of Kind.Version.Filter.t
  | ExistsSource of source

and version = version_predicate Formula.t
[@@deriving eq, show {with_path = false}, yojson]
