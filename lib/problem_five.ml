let rev lst =
  let rec aux acc = function
    | x :: tl -> aux (x :: acc) tl
    | [] -> acc
  in
  aux [] lst
;;

let%test _ = rev [ "a"; "b"; "c"; "d" ] = [ "d"; "c"; "b"; "a" ]
let%test _ = rev [] = []
let%test _ = rev [ "a" ] = [ "a" ]
