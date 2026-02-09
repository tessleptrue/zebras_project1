(*!tests!
 *
 * { "output": ["1"] }
 *
 *)

let rec f x = let x = x - 1 in if x = 0 then 1 else f x ;;
f 2 ;;