type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | One x :: tl -> aux (x :: acc) tl
    | Many l :: tl -> aux (aux acc l) tl
    | [] -> acc
  in
  List.rev (aux [] lst)
;;

let%test _ =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
  = [ "a"; "b"; "c"; "d"; "e" ]
;;
