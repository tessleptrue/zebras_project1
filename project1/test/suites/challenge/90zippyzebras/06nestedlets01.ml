(*!tests!
 *
 * { "output": ["10"] }
 *
 *)

let x = 10 in
  let y =
    let z =
      let x = 5 in
      x + 1
    in
    z + 3
  in x ;;
