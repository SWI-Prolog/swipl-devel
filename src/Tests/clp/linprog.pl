:- module(linprog, [linprog/0]).

:- dynamic user:file_search_path/2.
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../../../packages/clpr', Lib0),
   absolute_file_name(Lib0, Lib),
   asserta(user:file_search_path(library, Lib)).

:- use_module(library('clp/simplex')).

linprog :-
	radiation,
	transport.

radiation :-
	gen_state(S0),
	constraint([0.3*x1, 0.1*x2] =< 2.7, S0, S1),
	constraint([0.5*x1, 0.5*x2] = 6, S1, S2),
	constraint([0.6*x1, 0.4*x2] >= 6, S2, S3),
	constraint([x1] >= 0, S3, S4),
	constraint([x2] >= 0, S4, S5),
	minimize([0.4*x1, 0.5*x2], S5, S),
	Obj is 21 rdiv 4,
	X1 is 15 rdiv 2,
	X2 is 9 rdiv 2,
	objective(S, Obj),
	variable_value(S, x1, X1),
	variable_value(S, x2, X2).

transport :-
	transportation([12, 7, 14], [3, 15, 9, 6],
		[[20, 50, 10, 60],
		 [70, 40, 60, 30],
		 [40, 80, 70, 40]], Matrix),
	Matrix == [[0, 3, 9, 0],
		   [0, 7, 0, 0],
		   [3, 5, 0, 6]].

