signature ORD_KEY = sig
    type ord_key;
    val compare : ord_key * ord_key -> order;
end;

signature SIG = sig
    type symbol;                                      
    val arity : symbol -> int;                           
    structure ORD : ORD_KEY where type ord_key = symbol; (* structure ORD to compare functional symbols *)
end;

signature Var = sig
    type var;                                         
    structure ORD : ORD_KEY where type ord_key = var;
end;

functor Unify (structure S : SIG 
	   structure V : Var) = struct
	   datatype term = 	s of S.symbol
	   	|	v of V.var
	   	|	app of S.symbol * (term list);
	   
