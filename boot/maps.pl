:- module('$maps',
	  [ '.'/3				% +Left, +Right, -Result
	  ]).

replace_functions(Var, true, Var) :-
	var(Var), !.
replace_functions(.(L0,R0), Eval, V) :- !,
	replace_functions(L0, EvalL, L),
	replace_functions(R0, EvalR, R),
	conj(EvalL, EvalR, C0),
	conj(C0, .(L,R,V), Eval).
replace_functions(Term0, Eval, Term) :-
	compound(Term0), !,
	compound_name_arity(Term0, Name, Arity),
	compound_name_arity(Term, Name, Arity),
	map_functions(0, Arity, Term0, Term, Eval).
replace_functions(Term, true, Term).

map_functions(Arity, Arity, _, _, true) :- !.
map_functions(I0, Arity, Term0, Term, Eval) :-
	I is I0+1,
	arg(I, Term0, Arg0),
	arg(I, Term, Arg),
	replace_functions(Arg0, Eval0, Arg),
	map_functions(I, Arity, Term0, Term, Eval1),
	conj(Eval0, Eval1, Eval).

conj(true, X, X) :- !.
conj(X, true, X) :- !.
conj(X, Y, (X,Y)).

%%	.(+R, +L, -Result)
%
%	Evaluate dot expressions

.(Map, Func, Value) :-
	is_map(Map, Class), !,
	(   (atom(Func) ; var(Func))
	->  map_get(Map, Func, Value)
	;   Func = put(Name,New)
	->  map_put(Map, Name, New, Value)
	;   compound(Func)
	->  call(Class:Func, Map, Value)
	).
.(KV, Func, Value) :-
	is_list(KV), !,
	(   (atom(Func) ; var(Func))
	->  map_create(Map, _, KV),
	    map_get(Map, Func, Value)
	;   '$type_error'(atom, Func)
	).
.(Obj, _, _) :-
	'$type_error'(map, Obj).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

system:goal_expansion(G0, G) :-
	replace_functions(G0, Eval, G1),
	Eval \== true,
        (   var(G1)
        ->  G = Eval
	;   G = (Eval,G1)
	).

system:term_expansion((.(R,M) => V0 :- Body),
		      (Head :- Body, Eval)) :- !,
	replace_functions(V0, Eval, V),
	compound_name_arguments(M, Name, Args0),
	append(Args0, [R,V], Args),
	compound_name_arguments(M, Name, Args).
system:term_expansion((.(R,M) => V0),
		      (Head :- Eval)) :-
	replace_functions(V0, Eval, V),
	compound_name_arguments(M, Name, Args0),
	append(Args0, [R,V], Args),
	compound_name_arguments(M, Name, Args).
