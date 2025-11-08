let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl
;;

let%test _ = last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d")
let%test _ = last_two [ "a" ] = None
