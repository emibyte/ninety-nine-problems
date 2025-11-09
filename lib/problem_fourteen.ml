let duplicate lst =
  let rec aux acc = function
    | x :: tl -> aux (x :: x :: acc) tl
    | [] -> acc
  in
  List.rev (aux [] lst)
;;

let%test _ = duplicate [ "a"; "b"; "c"; "d" ] = [ "a"; "a"; "b"; "b"; "c"; "c"; "d"; "d" ]
