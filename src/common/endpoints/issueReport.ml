open Nes

module Request = struct
  type t = {
    reporter: (ModelBuilder.Core.User.t Entry.t, string) either;
    (** either a connected user, or a self description *)
    page: string; (* FIXME: Uri.t *)
    source_is_dancelor: bool;
    (** whether the problem comes from the Dancelor software or the model on the page *)
    title: string;
    description: string;
  }
  [@@deriving yojson]
end
type request = Request.t

module Response = struct
  type t = {
    title: string;
    id: int;
    uri: string; (* FIXME: Uri.t *)
  }
  [@@deriving yojson]
end
type response = Response.t
