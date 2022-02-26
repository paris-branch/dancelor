(* Extensions to existing modules. *)

include NesPervasives
module Char = NesChar
module Filename = NesFilename
module Format = NesFormat
module Int = NesInt
module Int64 = NesInt64
module List = NesList
module Lwt = NesLwt
module Lwt_list = NesLwt_list
module Seq = NesSeq
module String = NesString
module Option = NesOption

(* New modules *)

module Cache = NesCache
module Date = NesDate
module Filesystem = NesFilesystem
module Json = NesJson
module Slug = NesSlug

(* Monads *)

module Olwt = NesOlwt
module Rlwt = NesRlwt
module Rlwt_list = NesRlwt_list

(* Syntax *)

module Syntax = NesSyntax
include Syntax
