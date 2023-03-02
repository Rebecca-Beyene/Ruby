type 'a tree = 
  | Leaf 
  | Node of 'a tree * 'a * 'a tree

let rec map f xs = 
  match xs with
      [] -> []
    | h::t -> (f h)::(map f t)

let rec fold f a lst =
    match lst with
    []->a
    |h::t->fold f (f a h) t

let rec fold_right f lst a =
    match lst with
    []->a
    |h::t-> f h (fold_right f t a)

let rec map_tree f t = 
    match t with
    |Leaf -> Leaf 
    |Node(l,v,r) -> Node(map_tree f l, f v, map_tree f r)     

let rec fold_tree f b t = 
    match t with
    |Leaf -> b
    |Node(l, v, r)-> let l' = fold_tree f b l in
                     let r' = fold_tree f b r in 
                     f l' v r'  

(* for use of file in utop *)
(* #use "./file.m;";; *)

let add1 tree = map_tree (fun x -> x + 1) tree

let sum tree = fold_tree (fun la v ra -> la + v + ra) 0 tree

sum (add1 tree)

(*for reversing a list*)
fold (fun x a ->x::a) [] a