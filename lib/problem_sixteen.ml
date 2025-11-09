let drop_nth lst n =
  let is_nth i = i mod n = 0 in
  let rec aux acc i = function
    | x :: tl -> if is_nth i then aux acc (i + 1) tl else aux (x :: acc) (i + 1) tl
    | [] -> acc
  in
  List.rev (aux [] 1 lst)
;;

let%test _ =
  drop_nth [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
;;
