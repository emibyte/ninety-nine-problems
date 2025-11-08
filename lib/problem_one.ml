let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> last tl
;;

let%test _ = last [ "a"; "b"; "c"; "d" ] = Some "d"
let%test _ = last [] = None
