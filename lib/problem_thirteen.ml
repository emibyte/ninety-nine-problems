(* i already did it that way in problem 11 (not creating sublists and instead just consing directly) *)
let encode_direct lst = Problem_eleven.encode lst

let%test _ =
  encode_direct [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;
