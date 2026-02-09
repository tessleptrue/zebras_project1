(*!tests!
 *
 * { "output": ["16"] }
 *
 *)
let rec f x = let y = x + 1 in y * y ;;
f 3 ;;