(*
	foldr : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary
	foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary
```
The semantics of these functions are the following.
```
foldr sfun s0 [x0, x1, ... , xn] = f (x0 , f (x1, ... f (xn, s0))
foldl sfun s0 [x0, x1, ... , xn] = f (xn , f (x1, f ( x0, s0)))
```
*)


(* 1) Define the functions `foldr` and `foldl` using the pattern matching
   for list. *)



fun foldl sfun s []        = s		(*  val foldl = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
  | foldl sfun s (x :: xs) = foldl sfun (sfun (x, s)) xs;

fun foldr sfun s []        = s	   	(*  val foldr = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
  | foldr sfun s (x :: xs) = sfun (x, (foldr sfun s xs));




(* 2) Without using pattern matching, define the function `sum : int list
   -> int` that computes the sum of a list of integers.   *)

fun add (u,v)   = u + v;                    (*  val add = fn: int * int → int;  *)
fun sum xs      = foldl add 0 xs;           (*  val sum = fn: int list → int;   *)


(* 3) Instead of using explicit recursion, define the following library
function in terms of either foldr or foldl which ever is
convenient. For the documentation of these library function
*)


(*      val partition = fn : ('a -> bool) -> 'a list -> 'a list * 'a list     *)
fun partition pred xs = let
                            fun sfun (u, (x, y)) = case pred u of
                                                         true   => (u :: x, y)
                                                       | false  => (x, u :: y);
                        in
                            foldr sfun ([], []) xs
                        end;
                        
 
(*	 val map = fn : ('a -> 'b) -> 'a list -> 'b list           *)
fun map f xs = let
                   fun sfun (u, v) = f u :: v
               in
                   foldr sfun [] xs
               end;
               

(* 	val rev = fn : 'a list -> 'a list 		*)
fun rev xs = let
                 fun sfun (u, v) = u :: v
             in
                 foldl sfun [] xs
             end;
             


datatype 'a option = SOME of 'a
       | NONE;


datatype 'a Find = LookingFor of int
                 | Found      of 'a
                 
(*  val sfun = fn: 'a * 'a Find → 'a Find; *)
fun sfun (u, Found v)      = Found v
                           | sfun (u, LookingFor v) = case v of
                                                           0  =>  Found u
                                                         | v  =>  LookingFor (v - 1);


(* 	val nthAux = fn : 'a list * int -> 'a Find	 *)
fun nthAux (u, v) = foldl sfun (LookingFor v) u;


(*          nth : 'a list * int -> 'a option.		*)
fun nth (a, b)   = case (nthAux (a, b)) of
                         LookingFor v => NONE
                       | Found v      => SOME v;
