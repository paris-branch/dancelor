open Nes

module Build (Getters : Getters.S) = struct
  include Core.Any

  let equal any1 any2 =
    match any1, any2 with
    | Source c1, Source c2 -> Entry.equal' c1 c2
    | Person c1, Person c2 -> Entry.equal' c1 c2
    | Dance d1, Dance d2 -> Entry.equal' d1 d2
    | Book b1, Book b2 -> Entry.equal' b1 b2
    | Set s1, Set s2 -> Entry.equal' s1 s2
    | Tune t1, Tune t2 -> Entry.equal' t1 t2
    | Version v1, Version v2 -> Entry.equal' v1 v2
    | User u1, User u2 -> Entry.equal' u1 u2
    | _ -> false

  let name = function
    | Source p -> lwt @@ NEString.to_string @@ Core.Source.name' p
    | Person p -> lwt @@ Core.Person.name' p
    | Dance d -> lwt @@ Core.Dance.one_name' d
    | Book b -> lwt @@ Core.Book.title' b
    | Set s -> lwt @@ Core.Set.name' s
    | Tune t -> lwt @@ Core.Tune.one_name' t
    | Version v -> (Core.Tune.one_name' % Option.get) <$> Getters.get_tune @@ Core.Version.tune @@ Entry.value v
    | User u -> lwt @@ Core.User.username' u
end
