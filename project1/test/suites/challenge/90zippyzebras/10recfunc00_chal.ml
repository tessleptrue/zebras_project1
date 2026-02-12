(*!tests!
 *
 * { "output": ["4"] }
 *
 *)
 
let f =
  let rec g = fun x ->
    if x = 1 then 0 else 1 + g (x - 1)
  in
  g
in
f 5 ;;
