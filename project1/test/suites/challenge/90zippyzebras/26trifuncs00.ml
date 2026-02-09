(*!tests!
 *
 * { "output": ["6"] }
 *)

let f = fun x ->
  fun y ->
    fun z -> x + y + z
in
f 1 2 3 ;;
