(* Define the arithmetic operations as a recursive type *)
type operation =
 | PLUS of operation * operation
 | MINUS of operation * operation
 | MULT of operation * operation
 | DIV of operation * operation
 | FLOAT of float

(* Define the monad *)
type 'a monad = 'a * string list

(* Define the monad operations *)
let (>>=) (m : 'a monad) (f : 'a  -> 'b monad) : 'b monad =
 let (x, s1) = m in
 let (y, s2) = f x in
 (y, s1 @ s2)

let return (x: 'a) : 'a monad =
 (x, [])

(* Math operations *)
let add x y =
 let result = x +. y in
  (result, ["Added " ^ string_of_float x ^ " and " ^ string_of_float y ^ " to get " ^ string_of_float result])

let subtract x y =
 let result = x -. y in
 (result, ["Subtracted " ^ string_of_float y ^ " from " ^ string_of_float x ^ " to get " ^ string_of_float result])

let multiply x y =
 let result = x *. y in
 (result, ["Multiplied " ^ string_of_float x ^ " and " ^ string_of_float y ^ " to get " ^ string_of_float result])

exception Division_by_zero

let divide x y =
 if y = 0. then
   raise Division_by_zero
 else
   let result = x /. y in
   (result, ["Divided " ^ string_of_float x ^ " by " ^ string_of_float y ^ " to get " ^ string_of_float result])

let rec eval (expr: operation) =
 match expr with
 | FLOAT x -> return x
 | PLUS (op1, op2) ->
  eval op1 >>= fun x ->
  eval op2 >>= fun y ->
  add x y
| MINUS (op1, op2) ->
  eval op1 >>= fun x ->
  eval op2 >>= fun y ->
  subtract x y
| MULT (op1, op2) ->
  eval op1 >>= fun x ->
  eval op2 >>= fun y ->
  multiply x y
| DIV (op1, op2) ->
  eval op1 >>= fun x ->
  eval op2 >>= fun y ->
  divide x y

let expression = MINUS(PLUS (MULT (FLOAT 4., FLOAT 5.), FLOAT 7.), MULT(FLOAT 8., FLOAT 2.))