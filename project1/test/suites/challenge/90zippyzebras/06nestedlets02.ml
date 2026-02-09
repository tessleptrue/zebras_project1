(*!tests!
 *
 * { "output": ["9"] }
 *
 *)

let x = 21 in
let x = 5 in
  (x + 1) + 
  (let x = 4 in
     let x = 3 in
       x) ;;
