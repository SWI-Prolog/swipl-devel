:- module('$dicts',
	  [ '.'/3				% +Left, +Right, -Result
	  ]).

%%	.(+R, +L, -Result)
%
%	Evaluate dot expressions

.(Dict, Func, Value) :-
	is_dict(Dict, Tag), !,
	(   (atomic(Func) ; var(Func))
	->  get_dict_ex(Func, Dict, Value)
	;   eval_dict_function(Func, Tag, Dict, Value)
	).
.(KV, Func, Value) :-
	is_list(KV), !,
	(   (atomic(Func) ; var(Func))
	->  dict_create(Dict, _, KV),
	    get_dict_ex(Func, Dict, Value)
	;   '$type_error'(atom, Func)
	).
.(Obj, _, _) :-
	'$type_error'(dict, Obj).


%%	eval_dict_function(+Func, +Tag, +Dict, -Value)
%
%	Test for predefined functions on dicts or evaluate a user-defined
%	function.

eval_dict_function(get(Key), _, Dict, Value) :- !,
	get_dict(Key, Dict, Value).
eval_dict_function(put(Key, Value), _, Dict, NewDict) :- !,
	(   atomic(Key)
	->  put_dict(Key, Dict, Value, NewDict)
	;   put_dict_path(Key, Dict, Value, NewDict)
	).
eval_dict_function(put(New), _, Dict, NewDict) :- !,
	put_dict(New, Dict, NewDict).
eval_dict_function(Func, Tag, Dict, Value) :-
	call(Tag:Func, Dict, Value).


%%	put_dict_path(+KeyPath, +Dict, +Value, -NewDict)
%
%	Add/replace  a  value  according  to  a  path  definition.  Path
%	segments are separated using '/'.

put_dict_path(Key, Dict, Value, NewDict) :-
	atom(Key), !,
	put_dict(Key, Dict, Value, NewDict).
put_dict_path(Path, Dict, Value, NewDict) :-
	get_dict_path(Path, Dict, _Old, NewDict, Value).

get_dict_path(Path/Key, Dict, Old, NewDict, New) :- !,
	get_dict_path(Path, Dict, OldD, NewDict, NewD),
	(   get_dict(Key, OldD, Old, NewD, New),
	    is_dict(Old)
	->  true
	;   Old = _{},
	    put_dict(Key, OldD, New, NewD)
	).
get_dict_path(Key, Dict, Old, NewDict, New) :-
	get_dict(Key, Dict, Old, NewDict, New),
	is_dict(Old), !.
get_dict_path(Key, Dict, _{}, NewDict, New) :-
	put_dict(Key, Dict, New, NewDict).


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
	'$append'(Args0, [R,V], Args),
	compound_name_arguments(Head, Name, Args).
system:term_expansion((.(R,M) := V0),
		      (Head :- Eval)) :-
	'$expand':replace_functions(V0, Eval, V, _Ctx),
	compound_name_arguments(M, Name, Args0),
	'$append'(Args0, [R,V], Args),
	compound_name_arguments(Head, Name, Args).
