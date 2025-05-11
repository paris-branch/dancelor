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
    | _ -> false

  let name = function
    | Source p -> Lwt.return @@ Core.Source.name' p
    | Person p -> Lwt.return @@ Core.Person.name' p
    | Dance d -> Lwt.return @@ Core.Dance.name' d
    | Book b -> Lwt.return @@ Core.Book.title' b
    | Set s -> Lwt.return @@ Core.Set.name' s
    | Tune t -> Lwt.return @@ Core.Tune.name' t
    | Version v -> Lwt.map Core.Tune.name' @@ Getters.get_tune @@ Core.Version.tune @@ Entry.value v
end
