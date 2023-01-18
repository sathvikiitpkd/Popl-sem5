(*   (1)For lists define the function map : ('a -> 'b) -> 'a list -> 'b list. The semantics of map is that it applies the given function on all the elements of the list *)

fun map f []      = []
  | map f (x::xs) = f x :: map f xs;  (* map = fn : ('a -> 'b) -> 'a list -> 'b list  *)


(*    (2) the data type 'a tree that captures a binary tree.   *)
datatype 'a Tree = nil
		 | node of (('a Tree) * 'a * ('a Tree));    (* 'a Tree = nil | node of 'a Tree * 'a * 'a Tree *)

(* fn : 'a -> 'a Tree *)
fun ston x = node(nil,x,nil);


(*    (3) a function treemap analogues to map for list  *)
fun treemap ft  nil                    =  nil 
  | treemap ft (node(t1,x,t2))         =  node(treemap ft t1 ,ft x,treemap ft t2);  (* treemap = fn : ('a -> 'b) -> 'a Tree -> 'b Tree *)


(*    (4) the in-order, pre-order and post-order traversal of the binary tree returning the list of nodes in the given order.  *) 

fun inorder nil = []
  | inorder (node(left,x,right)) = inorder(left) @ x :: inorder(right);             (* inorder = fn : 'a Tree -> 'a Tree *)

fun preorder nil = []
  | preorder (node(left,x,right)) = x :: preorder(left) @ preorder(right);          (* preorder = fn : 'a Tree -> 'a Tree *)

fun postorder nil = []
  | postorder (node(left,x,right)) = postorder(left) @ postorder(right)@  [x];      (* postorder = fn : 'a Tree -> 'a Tree *)



(*    (5)  the rotate clockwise function for binary trees*)

fun rotate nil                        = nil
  | rotate (node(nil,x,y))            = (node(nil,x,y)) 
  | rotate (node(node(t1,b,t2),a,t3)) = node(t1,b,node(t2,a,t3));                   (*rotate = fn : 'a Tree -> 'a Tree  *)
