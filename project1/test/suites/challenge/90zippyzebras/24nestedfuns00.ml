(*!tests!
 *
 * { "output": ["81"] }
 *
 *)

let rec f x = x * x ;;
f (f 3) ;;