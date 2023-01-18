(* Consider variants of lambda calculus λ-calculus ⊆ λ-let ⊆ λ-letrec where the former adds the syntactic sugar of non-recursive let and
the latter adds letrec. It adds the construct let x = e₁ in e₂ end and letrec x = e₁ in e₂ end. The difference in let and letrec is
that the former is a non-recursive let where are the later is recursive let.

In nonrecursive let let x = e₁ in e₂ any occurance of x in e₁ is those that are bound outside the let expression itself
For example in the code below
let x := 10 in               -- Line (1)
   let x := x + 1            -- Line (2)
    in x end                 -- Line (3)
end

The x in Line (3) is the x that is bound in line (2) whereas the x in line 2 on the RHS of the binding x := x +1 is the x bound in Line (1). In particular, the program
let x := x+1 in x end
should give an error because the x in the RHS of the x := x+1 binding is not bound.

In recursive let (called letrec) letrec x := e₁ in e₂ end the x that occurs in e₁ is the one that is bound in the letrec itself.

1) Define abstract syntax for λ-let and λ-letrec as a SML datatype.
2) Write the conversion λ-let to λ-calculus.

*)

type var = Atom.atom


datatype lambda = Var of var
                | Apply of lambda * lambda
                | Abs of var * lambda

(* Abstract syntax for λ-let as a SML datatype *)
datatype lambdalet = Var_let of var
                   | Apply_let of lambdalet * lambdalet
                   | Lambda_let of var * lambdalet
                   | Let of var * lambdalet * lambdalet

(* Abstract syntax for  λ-letrec as a SML datatype *)
datatype lambdaletrec = var_letrec of var
                      | Apply_letrec of lambdaletrec * lambdaletrec
                      | Lambda_letrec of var * lambdaletrec
                      | Letrec of var * lambdaletrec * lambdaletrec

(* val convert = fn : lambdalet -> lambda *)
fun convert (Var_let x)          = Var x
  | convert (Apply_let (e1, e2)) = Apply (convert e1, convert e2)
  | convert (Lambda_let (x, e))  = Abs (x, convert e)
  | convert (Let (x, e1, e2))    = Apply (Abs (x, convert e2), convert e1)
