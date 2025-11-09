let split lst len_first =
  let rec aux acc size = function
    | x :: tl -> if size <= len_first then aux (x :: acc) (size + 1) tl else acc, x :: tl
    | [] -> acc, []
  in
  let fst, snd = aux [] 1 lst in
  List.rev fst, snd
;;

let%test _ =
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
;;

let%test _ = split [ "a" ] 1 = ([ "a" ], [])
let%test _ = split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], [])
