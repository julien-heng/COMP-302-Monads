(* PROVIDED CODE *)
type 'a monad = 'a * string list;;

type operation =
 | PLUS of operation * operation
 | MINUS of operation * operation
 | MULT of operation * operation
 | DIV of operation * operation
 | FLOAT of float;;

type tree = Empty | Node of tree * int * tree;;

exception NotImplemented;;

(* Mathematical operations *)

(* 1. Implement Basic Operations *)
let add (x: float) (y: float) : float * string list =
  raise NotImplemented;;
let subtract (x: float) (y: float) : float * string list =
  raise NotImplemented;;
let multiply (x: float) (y: float) : float * string list =
  raise NotImplemented;;

exception Division_by_zero;;
 
let divide (x: float) (y: float) : float * string list =
  raise NotImplemented;;

(* 2. Monadic Structure *)

let (>>=) (m : 'a monad) (f : 'a  -> 'b monad) : 'b monad =
 raise NotImplemented;;

let return (x: 'a) : 'a monad =
  raise NotImplemented;;

(* 3. Evaluate Nested Expressions *)

let rec eval (expr: operation) : float monad =
  raise NotImplemented;;

(* DFS and Trees *)

(* 1. Simple DFS *)
let dfs (x: int) (g: tree) : bool =
  raise NotImplemented;;

(* 2. DFS with Monads *)

let log (m: 'a monad) (messages: string list) = (m >>= (fun x -> (x, messages)));;

let dfs_monad (x: int) (g: tree) : tree monad =
  raise NotImplemented;;