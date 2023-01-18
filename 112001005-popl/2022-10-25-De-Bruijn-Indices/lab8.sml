(*
    1) Capture the De Bruijn representation of lambda calculus as a SML data type
*)

(*
    datatype DeBruijinExpr
  = Apply of DeBruijinExpr * DeBruijinExpr
  | Lamdba of DeBruijinExpr
  | Var of int 
*)

datatype DeBruijinExpr = Var of int
                        |   Lamdba of DeBruijinExpr
                        |   Apply of DeBruijinExpr * DeBruijinExpr


(*datatype expr
    = apply of expr * expr 
    | lamdba of Atom.atom * expr 
    | var of Atom.atom *)
    
datatype expr = var of Atom.atom
                |   lamdba of Atom.atom * expr
                |   apply  of expr * expr

(*
    2) A Program to convert from De Bruijn index to the first order representation of lambda calculus expression.
*)

(* val diag = fn : string -> string -> string *)
fun diag x y =  if String.isPrefix x y then y^"s" else x^"s"

(* val diagA = fn : string -> Atom.atom -> string *)
fun diagA x y = let val str = Atom.toString(y) in diag x str end

(* val fresh = fn : AtomRedBlackSet.set -> string *)
fun fresh z = let
		            fun swap f (x,y) = f y x
        	    in
        		    AtomSet.foldr (swap diagA) ("") z
        	    end

(* val Convert = fn : string list -> AtomRedBlackSet.set -> DeBruijinExpr -> expr *)
fun   Convert xs x (Var expr)            = var (Atom.atom (List.nth (xs,expr-1)))
    | Convert xs x (Lamdba expr)         = lamdba ( Atom.atom (fresh x), Convert ((fresh x) :: xs) (AtomSet.add (x , Atom.atom (fresh x))) expr)
    | Convert xs x (Apply (expr1,expr2)) = apply (Convert xs x expr1, Convert xs x expr2)

(* val DeBrIndex = fn : DeBruijinExpr -> expr *)
fun DeBrIndex expr = Convert [] AtomSet.empty expr
