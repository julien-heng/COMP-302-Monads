(* PROVIDED CODE *)
type 'a monad = 'a * string list;;

type operation =
 | PLUS of operation * operation
 | MINUS of operation * operation
 | MULT of operation * operation
 | DIV of operation * operation
 | FLOAT of float;;

type tree = Empty | Node of tree * int * tree;;


(* Mathematical operations *)

(* 1. Implement Basic Operations *)
let add x y =
  let result = x +. y in
    (result, ["Added " ^ string_of_float x ^ " and " ^ string_of_float y ^ " to get " ^ string_of_float result]);;
 
let subtract x y =
  let result = x -. y in
    (result, ["Subtracted " ^ string_of_float y ^ " from " ^ string_of_float x ^ " to get " ^ string_of_float result]);;
 
let multiply x y =
  let result = x *. y in
    (result, ["Multiplied " ^ string_of_float x ^ " and " ^ string_of_float y ^ " to get " ^ string_of_float result]);;
 
exception Division_by_zero;;
 
let divide x y =
  if y = 0. then
    raise Division_by_zero
  else
    let result = x /. y in
      (result, ["Divided " ^ string_of_float x ^ " by " ^ string_of_float y ^ " to get " ^ string_of_float result]);;

(* 2. Monadic Structure *)

let (>>=) (m : 'a monad) (f : 'a  -> 'b monad) : 'b monad =
 let (x, s1) = m in
 let (y, s2) = f x in
 (y, s1 @ s2);;

let return (x: 'a) : 'a monad =
 (x, []);;

(* 3. Evaluate Nested Expressions *)

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
;;

(* DFS and Trees *)

(* 1. Simple DFS *)
let dfs (x: int) (g: tree) =
  let rec dfs' fc g' =
    match g' with
    | Empty -> fc ()
    | Node (l, v, r) -> 
      if v = x then true
      else 
        (dfs' (fun () -> dfs' fc r)) l
  in 
  dfs' (fun () -> false) g;;

(* 2. DFS with Monads *)

let log (m: 'a monad) (messages: string list) = (m >>= (fun x -> (x, messages)));;

let dfs_monad (x: int) (g: tree) =
  let rec dfs' fc g' =
    match g' with
    | Empty -> fc ("Empty")
    | Node (l, v, r) -> 
      let status = "Visiting " ^ (string_of_int v) in
      if v = x then (log (return g) ["Found " ^ (string_of_int v)])
      else 
        (log (return l) [status; "Will visit Left subtree"]) >>= dfs' 
            (fun s -> (log (return r) [s; "Go back to " ^ string_of_int v ; status; "Will visit Right subtree"]) >>= dfs' fc)
  in 
  return g >>= dfs' (fun (s) -> (log (return g) [s; "Not Found!"]));;