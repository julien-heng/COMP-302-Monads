type operation =

 | PLUS of operation list

 | MINUS of operation list

 | MULT of operation list

 | DIV of operation list

 | FLOAT of int

let ( >>= ) (m : int * string) (f : int -> int * string) : int * string =

 let (x, s1) = m in

 let (y, s2) = f x in

 (y, s1 ^ s2)

let return (x : int) : int * string =

 (x, "")

let add x y =

 let result = x + y in

 (result, Printf.sprintf "Added %d and %d to get %d\n" x y result)

let subtract x y =

 let result = x - y in

 (result, Printf.sprintf "Subtracted %d from %d to get %d\n" y x result)

let multiply x y =

 let result = x * y in

 (result, Printf.sprintf "Multiplied %d and %d to get %d\n" x y result)
let divide x y =

 if y = 0 then

   failwith "Division by zero"

 else

   let result = x / y in

   (result, Printf.sprintf "Divided %d by %d to get %d\n" x y result)

let rec calculate (expr: operation): int * string =

 match expr with

 | FLOAT x -> return x

 | PLUS ops ->

     List.fold_left (fun acc op ->

       acc >>= fun a ->

       calculate op >>= fun b ->

       add a b

     ) (return 0) ops

 | MINUS ops ->

     List.fold_left (fun acc op ->

       acc >>= fun a ->

       calculate op >>= fun b ->

       subtract a b

     ) (return 0) ops

 | MULT ops ->

     List.fold_left (fun acc op ->

       acc >>= fun a ->

       calculate op >>= fun b ->

       multiply a b

     ) (return 1) ops

 | DIV ops ->

     List.fold_left (fun acc op ->

       acc >>= fun a ->

       calculate op >>= fun b ->

       divide a b

     ) (return 1) ops

let compute_expression expression =

 calculate expression


let () =

 let expression = PLUS [MULT [FLOAT 4; FLOAT 5]; FLOAT 7] in

 let (result, log) = compute_expression expression in

 Printf.printf "Result: %d\nLog:\n%s" result log ;;
