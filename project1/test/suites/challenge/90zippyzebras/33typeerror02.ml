(*!tests!
 *
 * { "exception": "TypeError" }
 *
 *)

let rec f x = if x = 0 then 1 else f (x - 1) / 0 ;;
f 2 ;;