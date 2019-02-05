open Dancelor_common
open Js_of_ocaml

module Html = Dom_html

let js = Js.string

let format_args l =
  if l = [] then ""
  else begin
    List.map (fun (arg, value) -> arg ^ "=" ^ value) l
    |> String.concat "&"
    |> (fun s -> "?" ^ s)
  end

let send_request ?(prefix=Config.api_prefix) ?(meth="GET") ?(args=[]) 
  ~callback ~path () =
  let uri = 
    Printf.sprintf "/%s%s%s" prefix path (format_args args) 
  in
  let request = XmlHttpRequest.create () in
  request##.onreadystatechange := Js.wrap_callback (fun () -> 
    if request##.readyState = XmlHttpRequest.DONE 
    && request##.status = 200 then begin
      request##.responseText 
      |> Js.to_string 
      |> callback
    end);
  request##_open (js meth) (js uri) (Js._true);
  request##send Js.null
