type graph = Empty | Node of graph * int * graph;;
exception NotFound;;

let g = Node ( Node (Node(Empty, 8, Empty), 1, Node(Node(Empty, 5, Node (Empty, 10, Empty)), 9, Empty)), 2, Node (Node(Empty, 6, Empty), 3, Empty));;
let g_1 = Node(
    Node(
        Node(
            Node(Empty, 3, Empty), 
          2, 
            Node(Empty, 4, Empty)), 
      1, 
        Node(
            Node(Empty, 6, Empty), 
          5, 
            Node(Empty, 7, Empty))),
  0, 
    Node(
        Node(
              Node(Empty, 10, Empty), 
            9, 
              Node(Empty, 11, Empty)), 
      8, 
        Node(
              Node(Empty, 13, Empty), 
            12, 
              Node(Empty, 14, Empty))));;
(*
0
12
3456
789(10)

*)

type 'a monad = 'a * string list;;
let return x = (x, []);;
let (>>=) m f = 
  let (x, s) = m in
  let (y, s') = f x in
  (y, s @ s');;

module type Monad = sig
  type 'a monad
  val return : 'a -> 'a monad
  val (>>=) : 'a monad -> ('a -> 'b monad) -> 'b monad
end;;

let log (m: 'a monad) (messages: string list) = (m >>= (fun x -> (x, messages)));;

let dfs (g: graph) (x: int) =
  let rec dfs' fc g' =
    match g' with
    (*base case: Empty leave found, backtrack using fc*)
    | Empty -> fc ("Empty")
    (*recursive case: Node found*)
    | Node (l, v, r) -> 
      let status = "Visiting " ^ (string_of_int v) in
      (* if the Node is found*)
      if v = x then (g, ["Found " ^ (string_of_int v)])
      (* the Node has not been found yet*)
      else 
        (*Recursive call on left child, with fc being the recursive call on the right child*)
        (log (return l) [status; "Will visit Left subtree"]) >>= dfs' 
            (fun s -> (log (return r) [s; "Go back to " ^ string_of_int v ; status; "Will visit Right subtree"]) >>= dfs' fc)
  in 
  (*call inner function*)
  return g >>= dfs' (fun (s) -> (log (return g) [s; "Not Found!"]));;


let dfs_1 (g: graph) (x: int) =
  let rec dfs' fc g' =
    match g' with
    (*base case: Empty leave found, backtrack using fc*)
    | Empty -> fc ()
    (*recursive case: Node found*)
    | Node (l, v, r) -> 
      if v = x then true
      (* the Node has not been found yet*)
      else 
        (*Recursive call on left child, with fc being the recursive call on the right child*)
        (dfs' (fun () -> dfs' fc r)) l
  in 
  (*call inner function*)
  dfs' (fun () -> false) g;;