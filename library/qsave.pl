/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(qsave,
	  [ qsave_program/1
	  , qsave_program/2
	  ]).

:- module_transparent
	qsave_program/1,
	qsave_program/2.

:- system_mode(on).

:- dynamic verbose/1.

%	qsave_program(+File, +[Options ...])
%
%	Make a saved state in file `File'.

qsave_program(File) :-
	qsave_program(File, []).

qsave_program(FileSpec, Options0) :-
	'$strip_module'(FileSpec, Module, File),
	option(Options0, autoload/true, Autoload, Options1),
	option(Options1, map/[],        Map,      Options2),
	option(Options2, goal/[],       GoalTerm, Options3),
	option(Options3, op/save,	SaveOps,  Options4),
	(   GoalTerm == []
	->  Options = Options4
	;   term_to_atom(Module:GoalTerm, GoalAtom),
	    term_to_atom(GT, GoalAtom),
	    define_predicate(user:GT),
	    Options = [goal=GoalAtom|Options4]
	),
	(   Autoload == true
	->  save_autoload
	;   true
	),
	(   Map == []
	->  retractall(verbose(_))
	;   open(Map, write, Fd),
	    asserta(qsave:verbose(Fd))
	),
	set_feature(saved_program, true),
	$open_wic(File, Options),
	system_mode(on),		% generate system modules too
	save_modules,
	save_records,
	save_flags,
	save_imports,
	save_features,
	(   SaveOps == save
	->  save_operators
	;   true
	),
%	save_foreign_libraries,
	system_mode(off),
	$close_wic,
	(   nonvar(Fd)
	->  close(Fd)
	;   true
	).

save_modules :-
	forall(special_module(X), save_module(X)),
	forall((current_module(X), \+ special_module(X)), save_module(X)).

special_module(system).
special_module(user).

define_predicate(Head) :-
	'$define_predicate'(Head), !.	% autoloader
define_predicate(Head) :-
	'$strip_module'(Head, _, Term),
	functor(Term, Name, Arity),
	throw(error(existence_error(procedure, Name/Arity), _)).


		 /*******************************
		 *	      AUTOLOAD		*
		 *******************************/

save_autoload :-
	autoload.

		 /*******************************
		 *	       MODULES		*
		 *******************************/

save_module(M) :-
	$qlf_start_module(M),
	feedback('~n~nMODULE ~w~n', [M]),
	(   P = (M:H),
	    current_predicate(_, P),
	    \+ predicate_property(P, imported_from(_)),
	    \+ predicate_property(P, foreign),
	    functor(H, F, A),
	    feedback('~nsaving ~w/~d ', [F, A]),
	    save_attributes(P),
	    \+ predicate_property(P, (volatile)),
	    nth_clause(P, _, Ref),
	    feedback('.', []),
	    $qlf_assert_clause(Ref),
	    fail
	;   $qlf_end_part,
	    feedback('~n', [])
	).
	
pred_attrib(dynamic,       P, $set_predicate_attribute(P, dynamic,       1)).
pred_attrib(volatile,      P, $set_predicate_attribute(P, volatile,      1)).
pred_attrib(multifile,     P, $set_predicate_attribute(P, multifile,     1)).
pred_attrib(transparent,   P, $set_predicate_attribute(P, transparent,   1)).
pred_attrib(discontiguous, P, $set_predicate_attribute(P, discontiguous, 1)).
pred_attrib(notrace,       P, $set_predicate_attribute(P, trace,         0)).
pred_attrib(show_childs,   P, $set_predicate_attribute(P, hide_childs,   0)).
pred_attrib(indexed(Term), P, M:index(Term)) :-
	$strip_module(P, M, _).

save_attributes(P) :-
	pred_attrib(Attribute, P, D),
	predicate_property(P, Attribute),
	(   Attribute = indexed(Term)
	->  \+(( arg(1, Term, 1),
	         functor(Term, _, Arity),
		 forall(between(2, Arity, N), arg(N, Term, 0))))
	;   true
	),
	$add_directive_wic(D),
	feedback('(~w) ', [Attribute]), 
	fail.
save_attributes(_).

		 /*******************************
		 *	      RECORDS		*
		 *******************************/

save_records :-
	feedback('~nRECORDS~n', []),
	(   current_key(X),
	    feedback('~n~t~8|~w ', [X, V]),
	    recorded(X, V, _),
	    feedback('.', []),
	    $add_directive_wic(recordz(X, V, _)),
	    fail
	;   true
	).


		 /*******************************
		 *	      FLAGS		*
		 *******************************/

save_flags :-
	feedback('~nFLAGS~n~n', []),
	(   current_flag(X),
	    flag(X, V, V),
	    feedback('~t~8|~w = ~w~n', [X, V]),
	    $add_directive_wic(flag(X, _, V)),
	    fail
	;   true
	).

		 /*******************************
		 *	     IMPORTS		*
		 *******************************/

default_import(system, _, _) :- !, fail.
default_import(To, Head, _) :-
	$get_predicate_attribute(To:Head, (dynamic), 1), !,
	fail.
default_import(user, Head, _) :- !,
	$default_predicate(user:Head, system:Head).
default_import(To, Head, _From) :-
	$default_predicate(To:Head, user:Head).
default_import(To, Head, _From) :-
	$default_predicate(To:Head, system:Head).

save_imports :-
	feedback('~nIMPORTS~n~n', []),
	(   predicate_property(M:H, imported_from(I)),
	    \+ default_import(M, H, I),
	    functor(H, F, A),
	    feedback('~t~8|~w:~w/~d <-- ~w~n', [M, F, A, I]),
	    $add_directive_wic(M:import(I:H)),
	    fail
	;   true
	).	    

		 /*******************************
		 *	      FEATURES		*
		 *******************************/

save_features :-
	feedback('~nFEATURES~n~n', []),
	feature(Feature, Value),
	\+ c_feature(Feature),
	feedback('~t~8|~w: ~w~n', [Feature, Value]),
	$add_directive_wic(set_feature(Feature, Value)),
	fail.
save_features.

c_feature(symbol_file).
c_feature(compiled_at).
c_feature(min_integer).
c_feature(max_integer).
c_feature(min_tagged_integer).
c_feature(max_tagged_integer).
c_feature(pipe).
c_feature(readline).
c_feature(dynamic_stacks).
c_feature(open_shared_object).
c_feature(save_program).
c_feature(save).
c_feature(c_ldflags).
c_feature(c_cc).
c_feature(c_staticlibs).
c_feature(c_libs).
c_feature(home).
c_feature(version).
c_feature(arch).
c_feature(boot_file).
c_feature(unix).
c_feature(windows).
c_feature(max_arity).
c_feature(integer_rounding_function).
c_feature(bounded).

		 /*******************************
		 *	     OPERATORS		*
		 *******************************/

save_operators :-
	feedback('~nOPERATORS~n', []),
	findall(op(P, T, N), current_op(P, T, N), Ops),
	$reset_operators,
	make_operators(Ops, Set),
	findall(D, deleted_operator(Ops, D), Deleted),
	append(Set, Deleted, Modify),
	forall(member(O, Modify),
	       (   feedback('~n~t~8|~w ', [O]),
		   $add_directive_wic(O),
		   O)).

make_operators([], []).
make_operators([Op|L0], [Op|L]) :-
	Op = op(P, T, N),
	\+ current_op(P, T, N), !,
	make_operators(L0, L).
make_operators([_|T], L) :-
	make_operators(T, L).

deleted_operator(Ops, op(0, T, N)) :-
	current_op(_, T, N),
	\+ (  member(op(_, OT, N), Ops),
	      same_op_type(T, OT)
	   ).
	
same_op_type(T, OT) :-
	op_type(T, Type),
	op_type(OT, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).
op_type(xf,  postfix).
op_type(yf,  postfix).

		 /*******************************
		 *       FOREIGN LIBRARIES	*
		 *******************************/

save_foreign_libraries :-
	$c_current_predicate(_, shlib:reload_foreign_libraries), !,
	feedback('~nFOREIGN LIBRARY HOOK~n', []),
	$add_directive_wic(shlib:reload_foreign_libraries).
save_foreign_libraries.


		 /*******************************
		 *	       UTIL		*
		 *******************************/

feedback(Fmt, Args) :-
	verbose(Fd), !,
	format(Fd, Fmt, Args).
%	flush_output(Fd).		% Real debugging only
feedback(_, _).


option(List, Name/_Default, Value, Rest) :- % goal = Goal
	select(List, Name=Value, Rest), !.
option(List, Name/_Default, Value, Rest) :- % goal(Goal)
	Term =.. [Name, Value],
	select(List, Term, Rest), !.
option(List, _Name/Default, Default, List).
	
