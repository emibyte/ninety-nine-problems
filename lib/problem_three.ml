let rec at n = function
  | [] -> None
  | x :: tl -> if n = 1 then Some x else at (n - 1) tl
;;

let%test _ = at 3 [ "a"; "b"; "c"; "d"; "e" ] = Some "c"
let%test _ = at 3 [ "a" ] = None
