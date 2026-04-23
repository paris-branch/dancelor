open Nes
open Dancelor_common

let get id =
  match%lwt Globally_unique_id.get @@ Entry.Id.unsafe_coerce id with
  | None -> lwt_none
  | Some Book -> Option.map Model_builder.Core.Any.book <$> (Book.get @@ Entry.Id.unsafe_coerce id)
  | Some Dance -> Option.map Model_builder.Core.Any.dance <$> (Dance.get @@ Entry.Id.unsafe_coerce id)
  | Some Person -> Option.map Model_builder.Core.Any.person <$> (Person.get @@ Entry.Id.unsafe_coerce id)
  | Some Set -> Option.map Model_builder.Core.Any.set <$> (Set.get @@ Entry.Id.unsafe_coerce id)
  | Some Source -> Option.map Model_builder.Core.Any.source <$> (Source.get @@ Entry.Id.unsafe_coerce id)
  | Some Tune -> Option.map Model_builder.Core.Any.tune <$> (Tune.get @@ Entry.Id.unsafe_coerce id)
  | Some User -> Option.map Model_builder.Core.Any.user <$> (User.get @@ Entry.Id.unsafe_coerce id)
  | Some Version -> Option.map Model_builder.Core.Any.version <$> (Version.get @@ Entry.Id.unsafe_coerce id)
