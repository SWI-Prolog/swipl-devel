/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_xref,
	  [ xref_source/1,		% +Source
	    xref_called/2,		% ?Source, ?Callable
	    xref_defined/3,		% ?Source. ?Callable, How
	    xref_exported/2,		% ?Source, ?Callable
	    xref_clean/1,		% +Source
	    xref_current_source/1,	% ?Source
	    xref_built_in/1,		% ?Callable
	    xref_expand/2,		% +Term, -Expanded
	    xref_source_file/3,		% +Spec, -Path, +Source
	    xref_public_list/3,		% +Path, -Export, +Src
	    xref_meta/2			% +Goal, -Called
	  ]).
:- use_module(library(pce)).

:- dynamic
	called/2,			% called head
	(dynamic)/2,			% defined dynamic
	(multifile)/2,			% defined multifile
	defined/3,			% defined head
	imported/2,			% imported head
	exported/2,			% exported head
	source/1.

		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%	prolog:called_by(+Goal, -ListOfCalled)
%
%	If this succeeds, the cross-referencer assumes Goal may call any
%	of the goals in ListOfCalled. If this call fails, default
%	meta-goal analysis is used to determine additional called goals.

%	prolog:meta_goal(+Goal, -Pattern)
%
%	Define meta-predicates.  See the examples in this file for details.

:- multifile
	prolog:called_by/2,		% +Goal, -Called
	prolog:meta_goal/2.		% +Goal, -Pattern


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
	(   current_prolog_flag(xref, Xref)
	->  true
	;   Xref = false
	),
	set_prolog_flag(xref, true),
	asserta(user:message_hook(_,_,_), Ref),
	(   collect(Src)
	->  erase(Ref),
	    set_prolog_flag(xref, Xref)
	;   erase(Ref),
	    set_prolog_flag(xref, Xref),
	    fail
	).

xref_clean(Source) :-
	canonical_source(Source, Src),
	retractall(called(_, Src)),
	retractall(defined(_, Src, _Line)),
	retractall(exported(_, Src)),
	retractall(dynamic(_, Src)),
	retractall(multifile(_, Src)),
	retractall(source(Src)).
	
xref_current_source(Source) :-
	source(Source).

xref_called(Source, Called) :-
	canonical_source(Source, Src),
	called(Called, Src).

xref_defined(Source, Called, How) :-
	canonical_source(Source, Src),
	(   defined(Called, Src, Line),
	    How = local(Line)
	;   imported(Called, Src),
	    How = imported
	;   dynamic(Called, Src),
	    How = (dynamic)
	;   multifile(Called, Src),
	    How = (multifile)
	).

xref_exported(Source, Called) :-
	canonical_source(Source, Src),
	exported(Called, Src).

xref_built_in(Head) :-
	system_predicate(Head).

collect(Src) :-
	open_source(Src, Fd),
	'$style_check'(Old, Old),
	style_check(+dollar),
	repeat,
	    catch(read_term(Fd, Term,
			    [ character_escapes(true), % TBD: how to switch!?
			      term_position(TermPos)
			    ]), _, fail),
	    xref_expand(Term, T),
	    (   T == end_of_file
	    ->  !,
	        '$style_check'(_, Old),
	        close(Fd)
	    ;   arg(2, TermPos, Line),
		flag(xref_src_line, _, Line),
	        process(T, Src),
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
process_directive(multifile(Dynamic), Src) :-
	assert_multifile(Src, Dynamic).
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

xref_meta(G, Meta) :-			% call user extensions
	prolog:meta_goal(G, Meta).

xref_meta((A, B), 		[A, B]).
xref_meta((A; B), 		[A, B]).
xref_meta((A| B), 		[A, B]).
xref_meta((A -> B),		[A, B]).
xref_meta((A *-> B),		[A, B]).
xref_meta(findall(_V, G, _L),	[G]).
xref_meta(setof(_V, G, _L),	[G]).
xref_meta(bagof(_V, G, _L),	[G]).
xref_meta(forall(A, B),		[A, B]).
xref_meta(maplist(G, _L1, _L2),	[G+2]).
xref_meta(checklist(G, _L),	[G+1]).
xref_meta(sublist(G, _, _),	[G+1]).
xref_meta(call(G),		[G]).
xref_meta(call(G, _),		[G+1]).
xref_meta(call(G, _, _),	[G+2]).
xref_meta(call(G, _, _, _),	[G+3]).
xref_meta(call(G, _, _, _, _),	[G+4]).
xref_meta(not(G),		[G]).
xref_meta(notrace(G),		[G]).
xref_meta(\+(G),		[G]).
xref_meta(ignore(G),		[G]).
xref_meta(once(G),		[G]).
xref_meta(initialization(G),	[G]).
xref_meta(phrase(G, _A),	[G+2]).
xref_meta(phrase(G, _A, _R),	[G+2]).
xref_meta(catch(A, _, B),	[A, B]).
xref_meta(thread_create(A,_,_), [A]).
xref_meta(predsort(A,_,_),	[A+3]).
xref_meta(call_cleanup(A, B),	[A, B]).
xref_meta(call_cleanup(A, _, B),[A, B]).
xref_meta(on_signal(_,_,A),	[A+1]).

					% XPCE meta-predicates
xref_meta(pce_global(_, new(_)), _) :- !, fail.
xref_meta(pce_global(_, B),     [B+1]).
xref_meta(ifmaintainer(G),	[G]).	% used in manual
xref_meta(listen(_, G),		[G]).	% library(broadcast)
xref_meta(listen(_, _, G),	[G]).

process_body(Var, _) :-
	var(Var), !.
process_body(Goal, Src) :-
	prolog:called_by(Goal, Called), !,
	assert_called(Src, Goal),
	process_called_list(Called, Src).
process_body(Goal, Src) :-
	process_xpce_goal(Goal, Src), !.
process_body(Goal, Src) :-
	xref_meta(Goal, Metas), !,
	assert_called(Src, Goal),
	process_called_list(Metas, Src).
process_body(Goal, Src) :-
	assert_called(Src, Goal).

process_called_list([], _).
process_called_list([H|T], Src) :-
	process_meta(H, Src),
	process_called_list(T, Src).

process_meta(A+N, Src) :-
	callable(A), !,
	\+ A = _:_,
	A =.. List,
	length(Rest, N),
	append(List, Rest, NList),
	Term =.. NList,
	process_body(Term, Src).
process_meta(G, Src) :-
	process_body(G, Src).


		 /*******************************
		 *	    XPCE STUFF		*
		 *******************************/

pce_goal(send(_,_)).
pce_goal(get(_,_,_)).

process_xpce_goal(G, Src) :-
	pce_goal(G), !,
	assert_called(Src, G),
	(   term_member(Term, G),
	    compound(Term),
	    arg(1, Term, Prolog),
	    Prolog == @prolog,
	    (	Term =.. [message, _, Selector | T],
		atom(Selector)
	    ->	Called =.. [Selector|T],
		process_body(Called, Src)
	    ;	Term =.. [?, _, Selector | T],
		atom(Selector)
	    ->	append(T, [_R], T2),
	        Called =.. [Selector|T2],
		process_body(Called, Src)
	    ),
	    fail
	;   true
	).

term_member(X, X).
term_member(X, T) :-
	compound(T),
	arg(_, T, A),
	term_member(X, A).


		/********************************
		*       INCLUDED MODULES	*
		********************************/

process_use_module(_Module:_Files, _) :- !.	% loaded in another module
process_use_module([], _) :- !.
process_use_module([H|T], Src) :- !,
	process_use_module(H, Src),
	process_use_module(T, Src).
process_use_module(library(pce), Src) :- !,	% bit special
	xref_public_list(library(pce), Public, Src),
	forall(member(Name/Arity, Public),
	       (   functor(Term, Name, Arity),
		   \+ system_predicate(Term),
		   \+ Term = pce_error(_) 	% hack!?
	       ->  assert_import(Src, Name/Arity)
	       ;   true
	       )).
process_use_module(File, Src) :-
	(   xref_public_list(File, Public, Src)
	->  assert_import(Src, Public)
	;   true
	).

xref_public_list(File, Public, Src) :-
	xref_source_file(File, Source, Src),
	open(Source, read, Fd),
	catch(read(Fd, ModuleDecl), _, fail),
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
	defined(Goal, Src, _), !.
assert_defined(Src, Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	flag(xref_src_line, Line, Line),
	asserta(defined(Term, Src, Line)).

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
assert_dynamic(_, _M:_Name/_Arity) :- !. % not local
assert_dynamic(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(dynamic(Term, Src)).

assert_multifile(Src, (A, B)) :- !,
	assert_multifile(Src, A),
	assert_multifile(Src, B).
assert_multifile(_, _M:_Name/_Arity) :- !. % not local
assert_multifile(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(multifile(Term, Src)).


		/********************************
		*            UTILITIES		*
		********************************/

%	xref_source_file(+Spec, -File, +Src)
%	Find named source file.

xref_source_file(Plain, File, Src) :-
	atom(Plain),
	(   object(Src)
	->  get(Src?file, absolute_path, Path)
	;   Path = Src
	),
	file_directory_name(Path, Dir),
	concat_atom([Dir, /, Plain], Spec),
	do_xref_source_file(Spec, File), !.
xref_source_file(Spec, File, _) :-
	do_xref_source_file(Spec, File), !.
xref_source_file(Spec, _, _) :-
	verbose,
	print_message(warning, error(existence_error(file, Spec), _)),
	fail.

do_xref_source_file(Spec, File) :-
	absolute_file_name(Spec,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ], File), !.


open_source(Src, Fd) :-
	(   object(Src)
	->  pce_open(Src, read, Fd)
	;   open(Src, read, Fd)
	),
	(   peek_char(Fd, #)		% Deal with #! script
	->  skip(Fd, 10)
	;   true
	).

canonical_source(Object, Object) :-
	object(Object), !.
canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     file_errors(fail)
			   ],
			   Src).
