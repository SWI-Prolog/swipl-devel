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
	check_options(Options0),
	'$strip_module'(FileSpec, Module, File),
	option(Options0, autoload/true, Autoload,  Options1),
	option(Options1, map/[],        Map,       Options2),
	option(Options2, goal/[],       GoalTerm,  Options3),
	option(Options3, op/save,	SaveOps,   Options4),
	option(Options4, class/runtime, SaveClass, Options5),
	(   GoalTerm == []
	->  Options = Options5
	;   term_to_atom(Module:GoalTerm, GoalAtom),
	    term_to_atom(GT, GoalAtom),
	    define_predicate(user:GT),
	    Options = [goal=GoalAtom|Options5]
	),
	(   Autoload == true
	->  save_autoload
	;   true
	),
	open_map(Map),
	set_feature(saved_program, true),
	(   exists_file(File)
	->  delete_file(File)
	;   true
	),
	$rc_open_archive(File, RC),
	make_header(RC, SaveClass, Options),
	save_options(RC, [class(SaveClass)|Options]),
	save_resources(RC, SaveClass),
	$rc_open(RC, $state, $prolog, write, StateFd),
	$open_wic(StateFd),
	system_mode(on),		% generate system modules too
	save_modules(SaveClass),
	save_records,
	save_flags,
	save_imports,
	save_features,
	save_operators(SaveOps),
%	save_foreign_libraries,
	system_mode(off),
	$close_wic,
	close(StateFd),
	$rc_close_archive(RC),
	$mark_executable(File),
	close_map.

		 /*******************************
		 *	     HEADER		*
		 *******************************/

make_header(RC, _, Options) :-
	option(Options, emulator/(-), OptVal, _),
	OptVal \== -, !,
	absolute_file_name(OptVal, [access(read)], Emulator),
	$rc_append_file(RC, $header, $rc, none, Emulator).
make_header(RC, _, Options) :-
	option(Options, stand_alone/false, OptVal, _),
	OptVal == true, !,
	feature(symbol_file, Executable),
	$rc_append_file(RC, $header, $rc, none, Executable).
make_header(RC, SaveClass, _Options) :-
	feature(unix, true),
	feature(symbol_file, Executable),
	$rc_open(RC, $header, $rc, write, Fd),
	format(Fd, '#!/bin/sh~n', []),
	format(Fd, '# SWI-Prolog saved state~n', []),
	(   SaveClass == runtime
	->  ArgSep = ' -- '
	;   ArgSep = ' '
	),
	format(Fd, 'exec ${SWIPL-~w} -x "$0"~w"$@"~n~n', [Executable, ArgSep]),
	close(Fd).
make_header(_, _, _).


		 /*******************************
		 *	     OPTIONS		*
		 *******************************/

min_stack(local,    8192).
min_stack(global,   8192).
min_stack(trail,    8192).
min_stack(argument, 4096).
min_stack(heap,     204800).

convert_option(Stack, Val, NewVal) :-
	min_stack(Stack, Min), !,
	NewVal is max(Min, Val*1024).
convert_option(_, Val, Val).

save_options(RC, Options) :-
	$rc_open(RC, $options, $prolog, write, Fd),
	(   $option(OptionName, OptionVal0, _),
	        option(Options, OptionName/_, OptionVal1, _),
	        (   var(OptionVal1)	% used the default
		->  OptionVal = OptionVal0
		;   convert_option(OptionName, OptionVal1, OptionVal)
		),
	        format(Fd, '~w=~w~n', [OptionName, OptionVal]),
	    fail
	;   true
	),
	close(Fd).

		 /*******************************
		 *	     RESOURCES		*
		 *******************************/

save_resources(_RC, development) :- !.
save_resources(RC, _SaveClass) :-
	feedback('~nRESOURCES~n~n', []),
	copy_resources(RC),
	(   current_predicate(_, M:resource(_,_,_)),
	    forall(M:resource(Name, Class, FileSpec),
		   (   mkrcname(M, Name, RcName),
		       save_resource(RC, RcName, Class, FileSpec)
		   )),
	    fail
	;   true
	).

mkrcname(user, Name, Name) :- !.
mkrcname(M, Name, RcName) :-
	concat_atom([M, :, Name], RcName).

save_resource(RC, Name, Class, FileSpec) :-
	absolute_file_name(FileSpec, [access(read)], File), !,
	feedback('~t~8|~w~t~32|~w~t~48|~w~n',
		 [Name, Class, File]),
	$rc_append_file(RC, Name, Class, none, File).
save_resource(RC, Name, Class, _) :-
	$rc_handle(SystemRC),
	copy_resource(SystemRC, RC, Name, Class), !.
save_resource(_, Name, Class, FileSpec) :-
	$warning('Could not find resource ~w/~w on ~w or system resources',
		 [Name, Class, FileSpec]).

copy_resources(ToRC) :-
	$rc_handle(FromRC),
	$rc_members(FromRC, List),
	(   member(rc(Name, Class), List),
	    \+ user:resource(Name, Class, _),
	    \+ reserved_resource(Name, Class),
	    copy_resource(FromRC, ToRC, Name, Class),
	    fail
	;   true
	).

reserved_resource($header,	$rc).
reserved_resource($state,	$prolog).
reserved_resource($options,	$prolog).

copy_resource(FromRC, ToRC, Name, Class) :-
	$rc_open(FromRC, Name, Class, read, FdIn),
	$rc_open(ToRC,	 Name, Class, write, FdOut),
	feedback('~t~8|~w~t~24|~w~t~40|~w~n',
		 [Name, Class, '<Copied from running state>']),
	$copy_stream(FdIn, FdOut),
	close(FdOut),
	close(FdIn).


		 /*******************************
		 *	      MODULES		*
		 *******************************/

save_modules(SaveClass) :-
	forall(special_module(X),
	       save_module(X, SaveClass)),
	forall((current_module(X), \+ special_module(X)),
	       save_module(X, SaveClass)).

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

%	save_module(+Module, +SaveClass)
%
%	Saves a module

save_module(M, SaveClass) :-
	$qlf_start_module(M),
	feedback('~n~nMODULE ~w~n', [M]),
	(   P = (M:H),
	    current_predicate(_, P),
	    \+ predicate_property(P, imported_from(_)),
	    \+ predicate_property(P, foreign),
	    functor(H, F, A),
	    feedback('~nsaving ~w/~d ', [F, A]),
	    (	H = resource(_,_,_),
		SaveClass \== development
	    ->	save_attribute(P, (dynamic)),
		(   M == user
		->  save_attribute(P, (multifile))
		),
		feedback('(Skipped clauses)', []),
		fail
	    ;	true
	    ),
	    save_attributes(P),
	    \+ predicate_property(P, (volatile)),
	    nth_clause(P, _, Ref),
	    feedback('.', []),
	    $qlf_assert_clause(Ref, SaveClass),
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

predicate_attribute(P, Attribute) :-
	pred_attrib(Attribute, P, _),
	predicate_property(P, Attribute).

save_attribute(P, Attribute) :-
	pred_attrib(Attribute, P, D),
	(   Attribute = indexed(Term)
	->  \+(( arg(1, Term, 1),
	         functor(Term, _, Arity),
		 forall(between(2, Arity, N), arg(N, Term, 0))))
	;   true
	),
	$add_directive_wic(D),
	feedback('(~w) ', [Attribute]).

save_attributes(P) :-
	(   predicate_attribute(P, Attribute),
	    save_attribute(P, Attribute),
	    fail
	;   true
	).
	    

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

c_feature(arch).
c_feature(boot_file).
c_feature(bounded).
c_feature(c_cc).
c_feature(c_ldflags).
c_feature(c_libs).
c_feature(c_staticlibs).
c_feature(compiled_at).
c_feature(dynamic_stacks).
c_feature(home).
c_feature(integer_rounding_function).
c_feature(max_arity).
c_feature(max_integer).
c_feature(max_tagged_integer).
c_feature(min_integer).
c_feature(min_tagged_integer).
c_feature(open_shared_object).
c_feature(pipe).
c_feature(readline).
c_feature(resource_database).
c_feature(save).
c_feature(save_program).
c_feature(symbol_file).
c_feature(tty_control).
c_feature(unix).
c_feature(version).
c_feature(windows).

		 /*******************************
		 *	     OPERATORS		*
		 *******************************/

save_operators(true) :- !,
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
save_operators(_).

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

open_map([]) :- !,
	retractall(verbose(_)).
open_map(File) :-
	open(File, write, Fd),
	asserta(verbose(Fd)).

close_map :-
	retract(verbose(Fd)),
	close(Fd), !.
close_map.

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
	
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Option checking and exception generation.  This should be in a library!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

option_type(Name,	 integer) :- min_stack(Name, _MinValue).
option_type(class,	 atom([runtime,kernel,development])).
option_type(autoload,	 bool).
option_type(map,	 atom).
option_type(op,		 atom([save, standard])).
option_type(stand_alone, bool).
option_type(goal, 	 callable).
option_type(toplevel, 	 callable).
option_type(initfile, 	 atom).
option_type(emulator, 	 ground).

check_options([]).
check_options([Var|_]) :-
	var(Var),
	throw(error(domain_error(save_options, Var), _)).
check_options([Name=Value|T]) :-
	(   option_type(Name, Type)
	->  (   check_type(Type, Value)
	    ->  check_options(T)
	    ;	throw(error(domain_error(Type, Value), _))
	    )
	;   throw(error(domain_error(save_option, Name), _))
	).
check_options([Term|T]) :-
	Term =.. [Name,Arg],
	check_options([Name=Arg|T]).
check_options([Var|_]) :-
	var(Var),
	throw(error(domain_error(save_options, Var), _)).

check_type(integer, V) :-
	integer(V).
check_type(atom(List), V) :-
	atom(V),
	memberchk(V, List), !.
check_type(atom, V) :-
	atom(V).
check_type(callable, V) :-
	atom(V).
check_type(callable, V) :-
	compound(V).
check_type(ground, V) :-
	ground(V).
check_type(bool, true).
check_type(bool, false).
	
