:- module('$maps',
	  [ '.'/3				% +Left, +Right, -Result
	  ]).

%%	.(+R, +L, -Result)
%
%	Evaluate dot expressions

.(Map, Func, Value) :-
	is_map(Map, Class), !,
	(   (atom(Func) ; var(Func))
	->  get_map(Func, Map, Value)
	;   Func = put(Name,New)
	->  put_map(Name, Map, New, Value)
	;   Func = put(New)
	->  put_map(New, Map, Value)
	;   compound(Func)
	->  call(Class:Func, Map, Value)
	).
.(KV, Func, Value) :-
	is_list(KV), !,
	(   (atom(Func) ; var(Func))
	->  map_create(Map, _, KV),
	    get_map(Func, Map, Value)
	;   '$type_error'(atom, Func)
	).
.(Obj, _, _) :-
	'$type_error'(map, Obj).


		 /*******************************
		 *	       REGISTER		*
		 *******************************/

%%	system:term_expansion(+TermIn, -TermOut)
%
%	Support => syntax for defining new functions.

system:term_expansion((.(R,M) := V0 :- Body),
		      (Head :- Body, Eval)) :- !,
	'$expand':replace_functions(V0, Eval, V, _Ctx),
	compound_name_arguments(M, Name, Args0),
	append(Args0, [R,V], Args),
	compound_name_arguments(Head, Name, Args).
system:term_expansion((.(R,M) := V0),
		      (Head :- Eval)) :-
	'$expand':replace_functions(V0, Eval, V, _Ctx),
	compound_name_arguments(M, Name, Args0),
	append(Args0, [R,V], Args),
	compound_name_arguments(Head, Name, Args).
