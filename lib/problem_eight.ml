let rec compress = function
  | x :: (y :: _ as tl) -> if x = y then compress tl else x :: compress tl
  | smaller -> smaller
;;

let compress_constant_stack lst =
  let rec aux acc = function
    | x :: (y :: _ as tl) -> if x = y then aux acc tl else aux (x :: acc) tl
    | [ x ] -> aux (x :: acc) []
    | [] -> acc
  in
  List.rev (aux [] lst)
;;

let%test _ =
  compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

let%test _ =
  compress_constant_stack
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;
