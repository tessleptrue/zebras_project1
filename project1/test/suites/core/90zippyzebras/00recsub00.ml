(*!tests!
 *
 * { "output": ["3"] }
 *
 *)
let rec f x = if x = 0 then 3 else f (x - 1) ;;
f 5 ;; 
 