open Nes

type t =
  { person : Person.t Slug.t list [@default []] }
[@@deriving yojson, make]

let _key = "credit-filter"

let make ?person () =
  Lwt.return (make ?person ())

let person f = Lwt.return f.person

let only_person p = Lwt.return {person = [p]}
let add_person p f = Lwt.return {person = p :: f.person}
let remove_person p f = Lwt.return {person = List.filter ((<>) p) f.person}

module type S = sig
  type nonrec t = t

  val person : t -> Person.t list Lwt.t

  val only_person : Person.t -> t Lwt.t
  (** The filter consisting of only the given person. *)

  val add_person : Person.t -> t -> t Lwt.t
  val remove_person : Person.t -> t -> t Lwt.t

  val make :
    ?person:Person.t list ->
    unit -> t Lwt.t
end

let make ?person () =
  let%lwt person =
    match person with
    | None -> Lwt.return_none
    | Some person ->
      let%lwt person = Lwt_list.map_s Person.slug person in
      Lwt.return_some person
  in
  make ?person ()
