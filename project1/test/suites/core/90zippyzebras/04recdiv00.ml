(*!tests!
 *
 * { "output": ["10"] }
 *
 *)

 let rec f x = if x = 0 then 0 else 1 + f(x/2) ;; 
 f 1000 ;; 
