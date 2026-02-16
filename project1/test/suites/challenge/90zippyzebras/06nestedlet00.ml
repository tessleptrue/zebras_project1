(*!tests!
 *
 * { "output": ["2"] }
 *
 *)

let x = 1 in
  let x = 
    let x =
      let x =
        let x = 5 in x - 1
      in x - 1
    in x - 1
  in x ;;
