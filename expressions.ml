(* Define the arithmetic operations as a recursive type *)
type operation =
 | PLUS of operation list
 | MINUS of operation list
 | MULT of operation list
 | DIV of operation list
 | FLOAT of float

let (>>=) (m : float * string list) (f : float -> float * string list) : float * string list =
 let (x, s1) = m in
 let (y, s2) = f x in
 (y, s1 @ s2)

let return (x : float) : float * string list =
 (x, [])

(* Math operations *)
let add x y =
 let result = x +. y in
  (result, ["Added " ^ string_of_float x ^ " and " ^ string_of_float y ^ " to get " ^ string_of_float result])

let subtract x y =
 let result = x -. y in
 (result, ["Subtracted " ^ string_of_float x ^ " from " ^ string_of_float y ^ " to get " ^ string_of_float result])

let multiply x y =
 let result = x *. y in
 (result, ["Multiplied " ^ string_of_float x ^ " and " ^ string_of_float y ^ " to get " ^ string_of_float result])

let divide x y =
 if y = 0. then
   failwith "Division by zero"
 else
   let result = x /. y in
   (result, ["Divided " ^ string_of_float x ^ " by " ^ string_of_float y ^ " to get " ^ string_of_float result])

let rec calculate (expr: operation): float * string list =
 match expr with
 | FLOAT x -> return x
 | PLUS ops ->
    List.fold_left (fun acc op ->
       acc >>= fun a ->
       calculate op >>= fun b ->
       add a b
     ) (return 0.) ops
 | MINUS ops ->
     List.fold_left (fun acc op ->
       acc >>= fun a ->
       calculate op >>= fun b ->
       subtract a b
     ) (return 0.) ops
 | MULT ops ->
     List.fold_left (fun acc op ->
       acc >>= fun a ->
       calculate op >>= fun b ->
       multiply a b
     ) (return 1.) ops
 | DIV ops ->
     List.fold_left (fun acc op ->
       acc >>= fun a ->
       calculate op >>= fun b ->
       divide a b
     ) (return 1.) ops

let compute_expression expression =
 calculate expression

let expression = PLUS [MULT [FLOAT 4.; FLOAT 5.]; FLOAT 7.] 