(* Define the arithmetic operations as a recursive type *)
type operation =
  | Float of float
  | Plus
  | Minus
  | Mult
  | Div

(* Define the monadic structure for logging *)
let ( >>= ) (m : float * string) (f : float -> float * string) : float * string =
  let (x, log1) = m in
  let (y, log2) = f x in
  (y, log1 ^ log2)

let return (x : float) : float * string =
  (x, "")

(* Math operations *)
let add x y =
  let result = x +. y in
  (result, Printf.sprintf "Added %.2f and %.2f to get %.2f\n" x y result)

let subtract x y =
  let result = y -. x in
  (result, Printf.sprintf "Subtracted %.2f from %.2f to get %.2f\n" x y result)

let multiply x y =
  let result = x *. y in
  (result, Printf.sprintf "Multiplied %.2f and %.2f to get %.2f\n" x y result)

let divide x y =
  if y = 0.0 then
    failwith "Division by zero"
  else
    let result = y /. x in
    (result, Printf.sprintf "Divided %.2f by %.2f to get %.2f\n" y x result)

(* Function to compute an expression *)
let rec compute_expression expr =
  match expr with
  | [] -> failwith "Invalid expression"
  | [Float x] -> return x  (* Only one element, return it *)
  | Float x1 :: Float x2 :: Plus :: rest ->
      let sum = add x1 x2 in
      sum >>= fun s -> compute_expression (Float s :: rest)
  | Float x1 :: Float x2 :: Minus :: rest ->
      let diff = subtract x1 x2 in
      diff >>= fun d -> compute_expression (Float d :: rest)
  | Float x1 :: Float x2 :: Mult :: rest ->
      let prod = multiply x1 x2 in
      prod >>= fun p -> compute_expression (Float p :: rest)
  | Float x1 :: Float x2 :: Div :: rest ->
      let quot = divide x1 x2 in
      quot >>= fun q -> compute_expression (Float q :: rest)
  | _ -> failwith "Invalid expression"

(* Function to calculate a list of operations *)
let calculate (input : operation list) =
  compute_expression input;;

let my_expression = [Float 4.0; Float 5.0; Mult; Float 7.0; Plus];;

let (result, log) = calculate my_expression;;
Printf.printf "Result: %.2f\nLog:\n%s" result log;;
