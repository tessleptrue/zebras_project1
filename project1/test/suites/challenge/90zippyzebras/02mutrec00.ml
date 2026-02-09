(*!tests!
 *
 * { "output": ["true"] }
 *)

let even_odd =
  let rec even = fun x ->
    if x = 0 then true else odd (x - 1)
  and odd = fun x ->
    if x = 0 then false else even (x - 1)
  in
  fun n -> odd n
in
even_odd 5 ;;
