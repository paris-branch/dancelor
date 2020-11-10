(* Extensions to existing modules. *)

include NesPervasives
module Char = NesChar
module Filename = NesFilename
module Format = NesFormat
module List = NesList
module Lwt = NesLwt
module Lwt_list = NesLwt_list
module Seq = NesSeq
module String = NesString
module Option = NesOption

(* New modules *)

module Date = NesDate
module Filesystem = NesFilesystem
module Json = NesJson
module LwtOption = NesLwtOption
module LwtList = NesLwtList
module Slug = NesSlug

(* Syntax *)

include Syntax
include Option.Syntax
include Lwt.Syntax
include LwtOption.Syntax
