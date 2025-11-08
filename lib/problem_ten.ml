let encode lst =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | x :: (y :: _ as tl) ->
      if x = y then aux acc (count + 1) tl else aux ((count + 1, x) :: acc) 0 tl
  in
  List.rev (aux [] 0 lst)
;;

let%test _ =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;
