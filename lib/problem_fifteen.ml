let replicate lst factor =
  let rec prepend times element acc =
    if times = 0 then acc else prepend (times - 1) element (element :: acc)
  in
  let rec aux acc = function
    | x :: tl -> aux (prepend factor x acc) tl
    | [] -> acc
  in
  List.rev (aux [] lst)
;;

let%test _ =
  replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
;;
