(*
type var = Atom.atom
datatype expr
  = Apply of expr * expr | Lambda of Atom.atom * expr | Var of Atom.atom
*)

type var = Atom.atom

datatype expr = Var of var
	 |    Apply of expr * expr
	 |    Lambda of var * expr

(*	val free = fn : expr -> AtomRedBlackSet.set	*)
fun free (Var(x))       = AtomSet.singleton(x)
|   free (Apply(e1,e2)) = AtomSet.union(free e1 , free e2)
|   free (Lambda(y,e))  = AtomSet.subtract( free e, y)

(*	val bound = fn : expr -> AtomRedBlackSet.set	*)
fun bound (Var x)           = AtomSet.empty
|   bound (Apply (e1,e2))   = AtomSet.union(bound e1, bound e2)
|   bound (Lambda (y,e))    = AtomSet.union(bound e, AtomSet.singleton(y))

(*	val subst = fn : expr -> Atom.atom -> expr -> expr	*)
fun subst (e:expr) (x:Atom.atom) (Var (var:Atom.atom))          = if Atom.compare(x,var) = EQUAL then e else (Var var)
  | subst (e:expr) (x:Atom.atom) (Apply (e1, e2)) 		= Apply (subst e x e1, subst e x e2)
  | subst (e:expr) (x:Atom.atom) (Lambda (y:Atom.atom, e1))	=  if Atom.compare(x,y) = EQUAL then Lambda (y, e1) else Lambda (y, subst e x e1)

(*

val diag = fn : string -> string -> string
val diagA = fn : string -> Atom.atom -> string
val fresh = fn : AtomRedBlackSet.set -> Atom.atom

*)

fun diag x y = if String.isPrefix x y then y^"n" else x^"n"

fun diagA x y = let val str = Atom.toString(y) in diag x str end

fun fresh z =     let
		fun swap f (x,y) = f y x
	      in
		Atom.atom ( AtomSet.foldr (swap diagA) ("") z)
	      end
