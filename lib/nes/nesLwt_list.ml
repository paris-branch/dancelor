open NesPervasives.Syntax
open NesLwt.Syntax
include Lwt_list

let proj_sort_s ~proj compare =
  Lwt_list.map_s
    (fun elt -> Lwt.bind (proj elt)
        (fun p -> Lwt.return (elt, p)))
  >=>||
  (List.sort (fun (_, p1) (_, p2) -> compare p1 p2)
   ||> List.map fst
   ||> Lwt.return)
