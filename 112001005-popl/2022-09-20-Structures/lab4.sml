(* 1) Quick Sort Algorithm as a functor that takes the ordering
through its input structure O *)

signature SORT = sig
 	type t
 	val sort : t list -> t list
 end;

(* The ORD_KEY signature is a standard signature that is used
in defining various data structures like ordered map and
set.*)

 signature ORD_KEY =
 sig
 	type ord_key
     (* abstract type of keys that have a total order *)
 	val compare : ord_key * ord_key -> order
     (* comparison function for the ordering on keys *)
 end ;

functor QSort ( O : ORD_KEY): SORT =
struct  
   open O
   type t = O.ord_key
   fun comp u v = case O.compare(u,v) of          (*       val comp : t → t → bool       *)
                        LESS    => true
    |EQUAL   => true
    |GREATER => false;
   
   fun sort [] = []                                (*      val sort : t list -> t list    *)
    |  sort (x::xs) =  let 
                         val (left, right) = List.partition (fn y => comp y x) xs     (*        Here, val partition  : (t -> bool ) -> t list -> (t list * t list)  *)
                       in
                         sort left @ [x] @ sort right
                       end
  
end;


(* 2) a structure IntOrd by making use of the Int.compare
function. Use this to instantiate the QSort functor and get an
algorithm to sort list of integers.
*)

structure IntOrd : ORD_KEY =
struct
    type ord_key = int
    val compare = Int.compare     (*      val compare : int * int → order     *)
end;

(* For sorting a list of integers, We need to instantiate the QSort functor with the help of IntOrd structure *)
structure A = QSort(IntOrd);
(* By simply doing A.sort([1,6,3]),we get [1,3,6] *)