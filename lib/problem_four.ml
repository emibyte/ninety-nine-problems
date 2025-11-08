(* This function is tail-recursive: it uses a constant amount of
     stack memory regardless of list size. *)
let length lst =
  let rec aux cur_len = function
    | _ :: tl -> aux (cur_len + 1) tl
    | [] -> cur_len
  in
  aux 0 lst
;;

let%test _ = length [ "a"; "b"; "c" ] = 3
let%test _ = length [ "b"; "c" ] = 2
let%test _ = length [] = 0

(* not tail recursive *)
(* let rec length lst = *)
(*   match lst with *)
(*   | [] -> 0 *)
(*   | x :: tl -> length tl + 1 *)
