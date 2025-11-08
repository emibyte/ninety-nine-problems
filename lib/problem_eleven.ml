type 'a rle =
  | One of 'a
  | Many of int * 'a

let make_rle count value = if count = 1 then One value else Many (count, value)

let encode lst =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> Many (count + 1, x) :: acc
    | x :: (y :: _ as tl) ->
      if x = y then aux acc (count + 1) tl else aux (make_rle (count + 1) x :: acc) 0 tl
  in
  List.rev (aux [] 0 lst)
;;

let%test _ =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;
