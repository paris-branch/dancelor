include Sqlgg_traits.M with
type 'a connection = Postgresql.connection
and type Types.Bool.t = bool
and type Types.Int.t = int64
and type Types.UInt64.t = Unsigned.UInt64.t
and type Types.Float.t = float
and type Types.Text.t = string
and type Types.Blob.t = string
and type Types.Decimal.t = float
and type Types.Datetime.t = Nes.Datetime.t
and type Types.Json.t = Sqlgg_trait_types.json
and type Types.Json_path.t = Sqlgg_trait_types.json_path
and type Types.One_or_all.t = Sqlgg_trait_types.one_or_all
and type Types.Any.t = string
