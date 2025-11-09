type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode lst =
  let rec aux acc = function
    | One x :: tl -> aux (x :: acc) tl
    | Many (count, x) :: tl ->
      if count > 0 then aux (x :: acc) (Many (count - 1, x) :: tl) else aux acc tl
    | [] -> acc
  in
  List.rev (aux [] lst)
;;

let%test _ =
  decode [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
  = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;
