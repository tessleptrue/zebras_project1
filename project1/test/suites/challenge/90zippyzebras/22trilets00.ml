(*!tests!
 *
 * { "output": ["true"] }
 *
 *)

let x = true in ((let x = false in x) || (let x = true in x)) || x ;;