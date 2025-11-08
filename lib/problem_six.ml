open Problem_five

let is_palindrome lst = lst = rev lst
let%test _ = is_palindrome [ "x"; "a"; "m"; "a"; "x" ] = true
let%test _ = is_palindrome [ "a"; "b" ] = false
