/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_require,
	  [ pce_require/1		% file
	  , pce_require/3		% file x directive x message
	  , emacs_require/1		% from GNU-Emacs
	  , pce_require_all/1		% from file wildcard
	  ]).

:- use_module(library(pce)).
:- require([ '$warning'/2
	   , append/3
	   , call_emacs/2
	   , concat_atom/2
	   , exists_file/1
	   , expand_file_name/2
	   , forall/2
	   , ignore/1
	   , member/2
	   , sformat/3
	   ]).

target_prolog(quintus).
:- consult('xref/quintus.def').		% built-in's

:- dynamic
	called/1,			% called head
	defined/1,			% defined head
	compiling_class/1,			% classname
	output_to/2,			% output is emacs buffer
	current_require_declaration/1.	% Current declaration

		/********************************
		*            TOPLEVEL		*
		********************************/

pce_require(File) :-
	clean,
	collect(File),
	report.


pce_require(File, Directive, Message) :-
	new(D, string),
	new(M, string),
	asserta(output_to(D, M), Clause),
	pce_require(File),
	erase(Clause),
	get(D, value, Directive),
	get(M, value, Message).


emacs_require(File) :-
	pce_require(File, Directive, Message),
	call_emacs('(insert "~w")', Directive),
	(   Message \== ''
	->  call_emacs('(message "~w")', Message)
	;   true
	).


pce_require_all(Pattern) :-
	expand_file_name(Pattern, Files),
	member(File, Files),
	format('*** ~w ***~n', File),
	pce_require(File),
	fail ; true.


clean :-
	retractall(called(_)),
	retractall(defined(_)),
	retractall(compiling_class(_)),
	retractall(current_require_declaration(_)).
	

collect(File) :-
	find_source_file(File, Source),
	seeing(Old), see(Source),
	repeat,
	    read(Term),
	    (   Term == end_of_file
	    ->  !, seen, see(Old)
	    ;   process(Term),
		fail
	    ).


process((:- Directive)) :- !,
	process_directive(Directive), !.
process(PceClassTerm) :-
	compiling_class,
	process_pce(PceClassTerm), !.
process((Head :- Body)) :- !,
	assert_defined(Head),
	process_body(Body).
process(Head) :-
	assert_defined(Head).

		/********************************
		 *           DIRECTIVES		*
		 ********************************/

process_directive(use_module(_Module, Import)) :-
	assert_import(Import).
process_directive(require(Import)) :-		% Include if report only
	assert_current_require_declaration(Import).
process_directive(use_module(Modules)) :-
	process_use_module(Modules).
process_directive(ensure_loaded(Modules)) :-
	process_use_module(Modules).
process_directive(dynamic(Dynamic)) :-
	assert_dynamic(Dynamic).

						  % BEGIN/END PceEmacs mode
process_directive(emacs_begin_mode(Mode, Super, Summary, KB, ST)) :-
	get(string('emacs_%s_mode', Mode), value, Class),
	get(string('emacs_%s_mode', Super), value, SuperClass),
	process_directive(pce_begin_class(Class, SuperClass, Summary)),
	process_body(emacs_begin_mode(Mode, Super, Summary, KB, ST)).
process_directive(emacs_extend_mode(Mode, KB)) :-
	get(string('emacs_%s_mode', Mode), value, Class),
	process_directive(pce_extend_class(Class)),
	process_body(emacs_extend_mode(Mode, KB)).
process_directive(emacs_end_mode) :-
	process_directive(pce_end_class),
	process_body(emacs_end_mode).

						  % BEGIN/END PceDraw shape
process_directive(draw_begin_shape(Class, Super, Summ, Recs)) :-
	process_directive(pce_begin_class(Class, Super, Summ)),
	process_body(draw_begin_shape(Class, Super, Summ, Recs)).
process_directive(draw_end_shape) :-
	process_directive(pce_end_class),
	process_body(draw_end_shape).

						  % BEGIN/END class
process_directive(pce_begin_class(Class, Super)) :-
	process_directive(pce_begin_class(Class, Super, "")).
process_directive(pce_begin_class(Class, _, _)) :-
	functor(Class, ClassName, _),
	asserta(compiling_class(ClassName)),
	pce_compile:push_compile_operators.
process_directive(pce_extend_class(ClassName)) :-
	asserta(compiling_class(ClassName)),
	pce_compile:push_compile_operators.
process_directive(pce_end_class) :-
	retractall(compiling_class(_)),
	pce_compile:pop_compile_operators.
process_directive(op(P, A, N)) :-
	op(P, A, N).			% should be local ...
process_directive(pce_compile:push_compile_operators) :-
	pce_compile:push_compile_operators.
process_directive(pce_compile:pop_compile_operators) :-
	pce_compile:pop_compile_operators.
process_directive(Goal) :-
	process_body(Goal).


	      /********************************
	      *          PCE EXPANDED		*
	      ********************************/

:- pce_compile:push_compile_operators.
						  % VARIABLES, RESOURCES, ETC.
process_pce(variable(Name, Type, Access)) :-
	process_pce(variable(Name, Type, Access, "")).
process_pce(variable(_Name, _Type, _Access, _Doc)).
process_pce(resource(Name, Type, Default)) :-
	process_pce(resource(Name, Type, Default, "")).
process_pce(resource(_Name, _Type, _Default, "")).
process_pce(handle(X, Y, Kind)) :-
	process_pce(handle(X, Y, Kind, @default)).
process_pce(handle(_X, _Y, _Kind, _Name)).
						  % METHODS
process_pce((_Head :-> _Doc::Body)) :- !,
	process_body(Body).
process_pce((_Head :-> Body)) :- !,
	process_body(Body).
process_pce((_Head :<- _Doc::Body)) :- !,
	process_body(Body).
process_pce((_Head :<- Body)) :- !,
	process_body(Body).

:- pce_compile:pop_compile_operators.

	      /********************************
	      *             BODY		*
	      ********************************/

meta_goal((A, B), 		[A, B]).
meta_goal((A; B), 		[A, B]).
meta_goal((A| B), 		[A, B]).
meta_goal((A -> B),		[A, B]).
meta_goal(findall(_V, G, _L),	[G]).
meta_goal(setof(_V, G, _L),	[G]).
meta_goal(bagof(_V, G, _L),	[G]).
meta_goal(forall(A, B),		[A, B]).
meta_goal(maplist(G, _L1, _L2),	[G+2]).
meta_goal(checklist(G, _L),	[G+1]).
meta_goal(call(G),		[G]).
meta_goal(call(G, _A1),		[G+1]).
meta_goal(call(G, _A1, _A2),	[G+2]).
meta_goal(not(G),		[G]).
meta_goal(\+(G),		[G]).
meta_goal(ignore(G),		[G]).
meta_goal(once(G),		[G]).
meta_goal(initialization(G),	[G]).

meta_goal(ifmaintainer(G),	[G]).	% used in manual

process_body(Goal) :-
	meta_goal(Goal, Metas), !,
	assert_called(Goal),
	process_called_list(Metas).
process_body(Goal) :-
	assert_called(Goal),
	ignore(check_goal(Goal)).

process_called_list([]).
process_called_list([H|T]) :-
	process_meta(H),
	process_called_list(T).

process_meta(A+N) :- !,
	A =.. List,
	length(Rest, N),
	append(List, Rest, NList),
	Term =.. NList,
	process_body(Term).
process_meta(G) :-
	process_body(G).


		/********************************
		*           CHECKING		*
		********************************/

check_goal(Goal) :-
	send_list_goal(Goal),
	output_compatibility('Send/[2,3] used with list argument').

send_list_goal(Goal) :-
	(   Goal = send(A, B)
	;   Goal = send(A, B, C)
	),
	member(X, [A,B,C]),
	nonvar(X),
	X = [_|_], !.


		/********************************
		*       INCLUDED MODULES	*
		********************************/

process_use_module(_Module:_Files) :- !. % loaded in another module
process_use_module([]) :- !.
process_use_module([H|T]) :- !,
	process_use_module(H),
	process_use_module(T).
process_use_module(File) :-
	find_source_file(File, Source),
	seeing(Old), see(Source),
	read(ModuleDecl),
	seen, see(Old),
	(   ModuleDecl = (:- module(_, Public))
	->  assert_import(Public)
	;   true
	).


		/********************************
		*       PHASE 1 ASSERTIONS	*
		********************************/

assert_called(Var) :-
	var(Var), !.
assert_called(Goal) :-
	called(Goal), !.
assert_called(Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	asserta(called(Term)).

assert_defined(Goal) :-
	defined(Goal), !.
assert_defined(Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	check_system_predicate(Term),
	asserta(defined(Term)).

assert_import([]) :- !.
assert_import([H|T]) :-
	assert_import(H),
	assert_import(T).
assert_import(Name/Arity) :-
	functor(Term, Name, Arity),
	assert_defined(Term).

assert_dynamic((A, B)) :- !,
	assert_dynamic(A),
	assert_dynamic(B).
assert_dynamic(Name/Arity) :-
	functor(Term, Name, Arity),
	assert_defined(Term).


assert_current_require_declaration([]).
assert_current_require_declaration([Name/Arity|Rest]) :-
	functor(Head, Name, Arity),
	assert(current_require_declaration(Head)),
	assert_current_require_declaration(Rest).
				   
check_system_predicate(Head) :-
	built_in(Head), !,
	functor(Head, Name, Arity),
	target_prolog(Prolog),
	source_warning('Redefined ~w system predicate: ~w/~d',
		       [Prolog, Name, Arity]).
check_system_predicate(Head) :-
	predicate_property(system:Head, built_in), !,
	functor(Head, Name, Arity),
	Prolog = 'SWI-Prolog',
	source_warning('Redefined ~w system predicate: ~w/~d',
		       [Prolog, Name, Arity]).
check_system_predicate(_).

		/********************************
		*             REPORT		*
		********************************/

undefined(Head) :-
	defined(Head), !, fail.
undefined(Head) :-
	built_in(Head), !, fail.
undefined(_).

report :-
	findall(Head, (called(Head), undefined(Head)), U0),
	sort(U0, Undefined),		% remove duplicates
	(   forall(member(U, Undefined), current_require_declaration(U)),
	    forall(current_require_declaration(U), member(U, Undefined))
	->  message(':- require/1: up-to-date')
	;   output(':- require([ '),
	    report_undefined(Undefined),
	    output(']).~n')
	).


report_undefined([]).
report_undefined([L]) :- !,
	functor(L, Name, Arity),
	output('~q/~d~n	   ', [Name, Arity]).
report_undefined([H|T]) :-
	functor(H, Name, Arity),
	output('~q/~d~n	   , ', [Name, Arity]),
	report_undefined(T).


		/********************************
		*            UTILITIES		*
		********************************/

%	compiling_class/0
%	Succeeds if we are compiling_class some PCE class

compiling_class :-
	compiling_class(_).
	

%	find_source_file(+Spec, -File)
%	Find named source file.

find_source_file(library(Spec), Path) :- !,
	library_directory(Dir),
	extension(Extension),
	concat_atom([Dir, /, Spec, Extension], Path),
	exists_file(Path), !.
find_source_file(Spec, Path) :-
	atom(Spec),
	name(Spec, [0'/|_]), !,			  % absolute path
	extension(Extension),
	concat_atom([Spec, Extension], Path),
	exists_file(Path), !.
find_source_file(Spec, Path) :-
	atom(Spec),
	source_directory(Dir),
	extension(Extension),
	concat_atom([Dir, '/', Spec, Extension], Path),
	exists_file(Path), !.

extension('.pl').
extension('').

source_directory(Dir) :-
	seeing(File),
	name(File, S0),
	append(S1, S2, S0),
	\+ member(0'/, S2),
	append(S3, "/", S1), !,
	name(Dir, S3).
source_directory('.').


		/********************************
		*            OUTPUT		*
		********************************/

output(Fmt) :-
	output(Fmt, []).
output(Fmt, Args) :-
	output_to(D, _), D \== @nil, !,
	sformat(Buf, Fmt, Args),
	send(D, append, string(Buf)).
output(Fmt, Args) :-
	format(Fmt, Args).


message(Fmt) :-
	message(Fmt, []).
message(Fmt, Args) :-
	output_to(_, Msg), Msg \== @nil, !,
	sformat(Buf, Fmt, Args),
	send(Msg, append, string(Buf)).
message(Fmt, Args) :-
	format(user_error, Fmt, Args),
	format(user_error, '~n', []).


output_compatibility(Fmt) :-
	output_compatibility(Fmt, []).
output_compatibility(Fmt, Args) :-
	source_warning(Fmt, Args).

source_warning(Fmt, Args) :-
	'$warning'(Fmt, Args).
