
(*   	(1)     curry and un-curry functions *)

      fun curry   f a b c   =   f(a,b,c)         (*  curry   :   ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)   *)
      fun uncurry f(a,b,c) =    f a b c	         (*  uncurry :   ('a -> 'b -> 'c -> d) -> ('a * 'b * 'c -> 'd)    *)
     


(*  	(2)    Function which project a tuple into its components  *)

      fun frst (a,b) = a          (*   frst : 'a * 'b -> 'a   *)      
      fun secd (a,b) = b          (*   secd : 'a * 'b -> 'b   *)




(*   	(3)     Function for finding the length of list  *)

      fun	length [] = 0
          | length (x::xs) = 1+length(xs)	(*    length : 'a list -> int     *)
    
    
    
    
(*       (4)  Reverse a list in O(n)       *)
      fun	helper    [] ys        =  ys		(*    helper : 'a list -> 'a list -> 'a list   *)
          |	helper    (x::xs) ys   =  helper xs (x::ys)
      fun   reverse   []      =  []			    (*    reverse : 'a list -> 'a list   *)
          | reverse   (x::xs) =  helper (x::xs) []  
    
      
          
(*   	(5)  the nth element in the fibonacci Sequence in O(n)   *)

      fun     help x y 1            =  y		                          (*   help : int -> int -> int -> int    *)
           |  help x y n            =  help y (x+y) (n-1)
           
      fun     fib 1 		=  0
           |  fib 2                 =  1
           |  fib n                 =  help 0 1 n-2	                      (*   fib : int -> int  *)
      
      
