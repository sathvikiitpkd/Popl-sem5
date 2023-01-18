datatype Expr = 	constant of real
	          | variable of Atom.atom
	          | Plus of Expr * Expr
	          | Mul of Expr * Expr

datatype Stmt = assign of Atom.atom * Expr
                   | Print of Expr



(* val eval      : Env -> Expr -> real option
    (* Evaluates an expression. The result is real option to take care of
	   cases when there are undefined variables *)
val execute   : Env -> Stmt -> Env
    (* Executes a single statement and returns the modified environment *)
val interpret : Program -> unit
    (* Run the program starting with an empty environment
	This is essentially a fold from the left.
	*)
*)

val env = AtomMap.empty
type Program = Stmt list

fun foldl f s []      = s	
 |  foldl f s (x::xs) = foldl f (f x s) xs

fun eval env (constant(b)) = SOME b
 |  eval env (variable(c)) = AtomMap.find(env,c) 
 |  eval env (Plus(u,v))   = let val answer = case (Option.isSome(eval env u)) of
				false       => NONE
			         |  true => case Option.isSome(eval env v) of
					  	  false        => NONE
						| true => SOME(Option.valOf(eval env u) + Option.valOf(eval env v))
		     in answer end
 |  eval env (Mul(u,v))    = let val answer = case (Option.isSome(eval env u)) of
				false                          => NONE
			         |  true => case Option.isSome(eval env v) of
					  	  false       => NONE
						| true => SOME ( Option.valOf(eval env u) * Option.valOf(eval env v) )
		     in answer end;
		     

fun frst a b = a

fun execute (assign ((b,c))) env = AtomMap.insert (env,b, Option.valOf (eval env c))
 |  execute (Print expr1) env    = frst env (print(Real.toString(Option.valOf (eval env expr1))))

fun interpret xs = foldl execute env xs
