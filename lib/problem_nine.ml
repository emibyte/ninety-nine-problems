(* TODO: look at this again *)
let pack lst =
  let rec aux acc cur = function
    | [] -> []
    | [ x ] -> (x :: cur) :: acc
    | x :: (y :: _ as tl) ->
      if x = y then aux acc (x :: cur) tl else aux ((x :: cur) :: acc) [] tl
  in
  List.rev (aux [] [] lst)
;;

let%test _ =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;
