(*!tests!
 *
 * { "exception": "UnboundVariable" }
 *
 *)

let g = fun x -> f x in
g 3 ;;
