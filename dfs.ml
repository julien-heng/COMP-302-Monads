type graph = Empty | Node of graph * int * graph;;
exception NotFound;;

let g = Node ( Node (Node(Empty, 8, Empty), 1, Node(Node(Empty, 5, Node (Empty, 10, Empty)), 9, Empty)), 2, Node (Node(Empty, 6, Empty), 3, Empty));;

type 'a monad = 'a * string list;;
let return x = (x, []);;
let (>>=) m f = 
  let (x, s) = m in
  let (y, s') = f x in
  (y, s @ s');;

let log (m: 'a monad) (message: string) = (m >>= (fun x -> (x, [message])));;

let rec binary_search g x =
  let rec binary_search' fc g' =
    match g' with
    | Empty -> fc ("Empty")
    | Node (l, v, r) -> 
      let status = "Visiting " ^ (string_of_int v) in
      if v = x then (g, ["Found " ^ (string_of_int v)])
      else 
        (log (log (return l) status) ("Will visit Left subtree")) >>= binary_search' 
            (fun s -> (log (log (log (log (return r) s)("Go back to " ^ string_of_int v)) status) "Will visit Right subtree") >>= binary_search' fc)
        in return g >>= binary_search' (fun (s) -> log (log (return g) s) "Not Found :/");;
