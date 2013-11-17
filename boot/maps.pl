:- module('$maps',
	  [ '.'/3				% +Left, +Right, -Result
	  ]).

%%	.(+R, +L, -Result)
%
%	Evaluate dot expressions

.(Map, Func, Value) :-
	is_map(Map, Class), !,
	(   (atomic(Func) ; var(Func))
	->  get_map_ex(Func, Map, Value)
	;   eval_map_function(Func, Class, Map, Value)
	).
.(KV, Func, Value) :-
	is_list(KV), !,
	(   (atomic(Func) ; var(Func))
	->  map_create(Map, _, KV),
	    get_map_ex(Func, Map, Value)
	;   '$type_error'(atom, Func)
	).
.(Obj, _, _) :-
	'$type_error'(map, Obj).


%%	eval_map_function(+Func, +Class, +Map, -Value)
%
%	Test for predefined functions on maps or evaluate a user-defined
%	function.

eval_map_function(get(Key), _, Map, Value) :- !,
	get_map(Key, Map, Value).
eval_map_function(put(Key, Value), _, Map, NewMap) :- !,
	put_map(Key, Map, Value, NewMap).
eval_map_function(put(New), _, Map, NewMap) :- !,
	put_map(New, Map, NewMap).
eval_map_function(Func, Class, Map, Value) :-
	call(Class:Func, Map, Value).


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
