/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_xref,
	  [ xref_source/1,		% +Source
	    xref_called/2,		% ?Source, ?Callable
	    xref_defined/3,		% ?Source. ?Callable, How
	    xref_exported/2,		% ?Source, ?Callable
	    xref_clean/1,		% +Source
	    xref_current_source/1,	% ?Source
	    xref_built_in/1,		% ?Callable
	    xref_expand/2		% +Term, -Expanded
	  ]).
:- use_module(library(pce)).

:- dynamic
	called/2,			% called head
	(dynamic)/2,			% defined dynamic
	defined/2,			% defined head
	imported/2,			% imported head
	exported/2,			% exported head
	source/1.

		 /*******************************
		 *	     BUILT-INS		*
		 *******************************/

:- use_module(library('xref/common')).	% Common built-in's

system_predicate(require(_)).
system_predicate(discontiguous(_)).
system_predicate(Head) :-
	built_in(Head).


		/********************************
		*            TOPLEVEL		*
		********************************/

:- dynamic
	verbose/0.

%verbose.

xref_source(Source) :-
	verbose, !,				% do not suppress messages
	canonical_source(Source, Src),
	xref_clean(Src),
	assert(source(Src)),
	collect(Src).
xref_source(Source) :-
	canonical_source(Source, Src),
	xref_clean(Src),
	assert(source(Src)),
	asserta(user:message_hook(_,_,_), Ref),
	(   collect(Src)
	->  erase(Ref)
	;   erase(Ref),
	    fail
	).

xref_clean(Source) :-
	canonical_source(Source, Src),
	retractall(called(_, Src)),
	retractall(defined(_, Src)),
	retractall(exported(_, Src)),
	retractall(source(Src)).
	
xref_current_source(Source) :-
	source(Source).

xref_called(Source, Called) :-
	canonical_source(Source, Src),
	called(Called, Src).

xref_defined(Source, Called, How) :-
	canonical_source(Source, Src),
	(   defined(Called, Src),
	    How = local
	;   imported(Called, Src),
	    How = imported
	;   dynamic(Called, Src),
	    How = dynamic
	).

xref_exported(Source, Called) :-
	canonical_source(Source, Src),
	exported(Called, Src).

xref_built_in(Head) :-
	system_predicate(Head).

collect(Src) :-
	open_source(Src, Fd),
	repeat,
	    catch(read_term(Fd, Term,
			    [ character_escapes(true) % TBD: how to switch!?
			    ]), _, fail),
	    xref_expand(Term, T),
	    (   T == end_of_file
	    ->  !, close(Fd)
	    ;   process(T, Src),
		fail
	    ).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%	xref_expand(+Term, -Expanded)
%
%	Do the term-expansion.  We have to pass require as we need it
%	for validation.  Otherwise we do term-expansion, handling all
%	of the XPCE class compiler as normal Prolog afterwards.

xref_expand((:- require(X)), (:- require(X))) :- !.
xref_expand(Term, _) :-
	requires_library(Term, Lib),
	ensure_loaded(user:Lib),
	fail.
xref_expand(Term, T) :-
	catch(expand_term(Term, Expanded), _, Expanded=Term),
	(   is_list(Expanded)
	->  member(T, Expanded)
	;   T = Expanded
	).


%	requires_library(+Term, -Library)
%
%	known expansion hooks.  Should be more dynamic!

requires_library((:- emacs_begin_mode(_,_,_,_,_)), library(emacs_extend)).
requires_library((:- draw_begin_shape(_,_,_,_)), library(pcedraw)).


		 /*******************************
		 *	     PROCESS		*
		 *******************************/

process((:- Directive), Src) :- !,
	process_directive(Directive, Src), !.
process((Head :- Body), Src) :- !,
	assert_defined(Src, Head),
	process_body(Body, Src).
process('$source_location'(_File, _Line):Clause, Src) :- !,
	process(Clause, Src).
process(Head, Src) :-
	assert_defined(Src, Head).

		/********************************
		 *           DIRECTIVES		*
		 ********************************/

process_directive(List, Src) :-
	is_list(List), !,
	process_directive(consult(List), Src).
process_directive(use_module(_Module, Import), Src) :-
	assert_import(Src, Import).
process_directive(use_module(Modules), Src) :-
	process_use_module(Modules, Src).
process_directive(consult(Modules), Src) :-
	process_use_module(Modules, Src).
process_directive(ensure_loaded(Modules), Src) :-
	process_use_module(Modules, Src).
process_directive(dynamic(Dynamic), Src) :-
	assert_dynamic(Src, Dynamic).
process_directive(module(_Module, Export), Src) :-
	assert_export(Src, Export).

process_directive(op(P, A, N), _) :-
	op(P, A, N).			% should be local ...
process_directive(style_check(-atom), _) :-
	style_check(-atom).		% should be local ...
process_directive(pce_expansion:push_compile_operators, _) :-
	pce_expansion:push_compile_operators.
process_directive(pce_expansion:pop_compile_operators, _) :-
	pce_expansion:pop_compile_operators.
process_directive(Goal, Src) :-
	process_body(Goal, Src).


	      /********************************
	      *             BODY	      *
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
meta_goal(call(G, _),		[G+1]).
meta_goal(call(G, _, _),	[G+2]).
meta_goal(call(G, _, _, _),	[G+3]).
meta_goal(call(G, _, _, _, _),	[G+4]).
meta_goal(not(G),		[G]).
meta_goal(\+(G),		[G]).
meta_goal(ignore(G),		[G]).
meta_goal(once(G),		[G]).
meta_goal(initialization(G),	[G]).
meta_goal(phrase(G, _A),	[G+2]).
meta_goal(phrase(G, _A, _R),	[G+2]).
meta_goal(catch(A, _, B),	[A, B]).
meta_goal(thread_create(A,_,_), [A]).
					% XPCE meta-predicates
meta_goal(pce_global(_, new(_)), _) :- !, fail.
meta_goal(pce_global(_, B),     [B+1]).
meta_goal(ifmaintainer(G),	[G]).	% used in manual
meta_goal(listen(_, G),		[G]).	% library(broadcast)
meta_goal(listen(_, _, G),	[G]).

process_body(Var, _) :-
	var(Var), !.
process_body(Goal, Src) :-
	meta_goal(Goal, Metas), !,
	assert_called(Src, Goal),
	process_called_list(Metas, Src).
process_body(Goal, Src) :-
	assert_called(Src, Goal).

process_called_list([], _).
process_called_list([H|T], Src) :-
	process_meta(H, Src),
	process_called_list(T, Src).

process_meta(A+N, Src) :-
	nonvar(A), !,
	\+ A = _:_,
	A =.. List,
	length(Rest, N),
	append(List, Rest, NList),
	Term =.. NList,
	process_body(Term, Src).
process_meta(G, Src) :-
	process_body(G, Src).


		/********************************
		*       INCLUDED MODULES	*
		********************************/

process_use_module(_Module:_Files, _) :- !.	% loaded in another module
process_use_module([], _) :- !.
process_use_module([H|T], Src) :- !,
	process_use_module(H, Src),
	process_use_module(T, Src).
process_use_module(library(pce), Src) :- !,	% bit special
	file_public_list(library(pce), Public, Src),
	forall(member(Name/Arity, Public),
	       (   functor(Term, Name, Arity),
		   \+ system_predicate(Term),
		   \+ Term = pce_error(_) 	% hack!?
	       ->  assert_import(Src, Name/Arity)
	       ;   true
	       )).
process_use_module(File, Src) :-
	(   file_public_list(File, Public, Src)
	->  assert_import(Src, Public)
	;   true
	).

file_public_list(File, Public, Src) :-
	find_source_file(File, Source, Src),
	open(Source, read, Fd),
	read(Fd, ModuleDecl),
	close(Fd),
	ModuleDecl = (:- module(_, Public)).


		/********************************
		*       PHASE 1 ASSERTIONS	*
		********************************/

assert_called(_, Var) :-
	var(Var), !.
assert_called(Src, Goal) :-
	called(Goal, Src), !.
assert_called(Src, Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	asserta(called(Term, Src)).

assert_defined(_, _Module:_Head) :- !.	% defining in another module.  Bah!
assert_defined(Src, Goal) :-
	defined(Goal, Src), !.
assert_defined(Src, Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	asserta(defined(Term, Src)).

assert_import(_, []) :- !.
assert_import(Src, [H|T]) :-
	assert_import(Src, H),
	assert_import(Src, T).
assert_import(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(imported(Term, Src)).

assert_export(_, []) :- !.
assert_export(Src, [H|T]) :-
	assert_export(Src, H),
	assert_export(Src, T).
assert_export(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(exported(Term, Src)).

assert_dynamic(Src, (A, B)) :- !,
	assert_dynamic(Src, A),
	assert_dynamic(Src, B).
assert_dynamic(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(dynamic(Term, Src)).


		/********************************
		*            UTILITIES		*
		********************************/

%	find_source_file(+Spec, -File, +Src)
%	Find named source file.

find_source_file(Plain, File, Src) :-
	atom(Plain),
	(   object(Src)
	->  get(Src?file, absolute_path, Path)
	;   Path = Src
	),
	file_directory_name(Path, Dir),
	concat_atom([Dir, /, Plain], Spec),
	do_find_source_file(Spec, File), !.
find_source_file(Spec, File, _) :-
	do_find_source_file(Spec, File), !.
find_source_file(Spec, _, _) :-
	term_to_atom(Spec, Atom),
	send(@pce, report, warning, 'Cannot find file from %s\n', Atom),
	fail.

do_find_source_file(Spec, File) :-
	absolute_file_name(Spec,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ], File), !.


open_source(Src, Fd) :-
	object(Src),
	pce_open(Src, read, Fd).
open_source(File, Fd) :-
	open(File, read, Fd).

canonical_source(Object, Object) :-
	object(Object), !.
canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     file_errors(fail)
			   ],
			   Src).
