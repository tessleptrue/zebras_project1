(*!tests!
 *
 * { "output": ["6"] }
 *
 *) 


let rec f x y = x * y ;;
(let g = f 3 in g 2) ;;

