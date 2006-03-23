/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pce_prolog_xref,
	  [ xref_source/1,		% +Source
	    xref_called/2,		% ?Source, ?Callable
	    xref_called/3,		% ?Source, ?Callable, ?By
	    xref_defined/3,		% ?Source. ?Callable, -How
	    xref_exported/2,		% ?Source, ?Callable
	    xref_module/2,		% ?Source, ?Module
	    xref_op/2,			% ?Source, ?Op
	    xref_clean/1,		% +Source
	    xref_current_source/1,	% ?Source
	    xref_done/2,		% +Source, -Time
	    xref_built_in/1,		% ?Callable
	    xref_expand/2,		% +Term, -Expanded
	    xref_source_file/3,		% +Spec, -Path, +Source
	    xref_public_list/4,		% +Path, -Export, +Src
	    xref_meta/2,		% +Goal, -Called
	    xref_hook/1,		% ?Callable
					% XPCE class references
	    xref_used_class/2,		% ?Source, ?ClassName
	    xref_defined_class/3	% ?Source, ?ClassName, -How
	  ]).
:- use_module(library(pce)).
:- use_module(library(operators)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- dynamic
	called/3,			% Head, Src, From
	(dynamic)/2,			% Head, Src
	(thread_local)/2,		% Head, Src
	(multifile)/2,			% Head, Src
	defined/3,			% Head, Src, Line
	foreign/3,			% Head, Src, Line
	constraint/3,			% Head, Src, Line
	imported/3,			% Head, Src, From
	exported/2,			% Head, Src
	xmodule/2,			% Module, Src
	xop/2,				% Src, Op
	source/2,			% Src, Time
	used_class/2,			% Name, Src
	defined_class/5,		% Name, Super, Summary, Src, Line
	(mode)/2.			% Mode, Src


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
:- dynamic
	meta_goal/2.

		 /*******************************
		 *	     BUILT-INS		*
		 *******************************/

:- [library('xref/common')].		% Common built-in's

system_predicate(require(_)).
system_predicate(discontiguous(_)).
system_predicate(Head) :-
	built_in(Head).


		/********************************
		*            TOPLEVEL		*
		********************************/

verbose :-
	debugging(xref).

xref_source(Source) :-
	canonical_source(Source, Src),
	xref_clean(Src),
	get_time(Now),
	assert(source(Src, Now)),
	xref_setup(State),
	call_cleanup(collect(Src), xref_cleanup(State)).

xref_setup(state(Xref, Ref, Style, SM)) :-
	(   current_prolog_flag(xref, Xref)
	->  true
	;   Xref = false
	),
	set_prolog_flag(xref, true),
	'$set_source_module'(SM, SM),
	'$style_check'(Style, Style),
	push_operators([]),
	(   verbose
	->  Ref = []
	;   asserta(user:message_hook(_,_,_), Ref)
	).
xref_cleanup(state(Xref, Ref, Style, SM)) :-
	set_prolog_flag(xref, Xref),
	(   Ref \== []
	->  erase(Ref)
	;   true
	),
	pop_operators,
	'$style_check'(_, Style),
	'$set_source_module'(_, SM).

%	xref_push_op(Source, +Prec, +Type, :Name)
%	
%	Define operators into the default source module and register
%	them to be undone by pop_operators/0.

xref_push_op(Src, P, T, N0) :- !,
	(   N0 = _:_
	->  N = N0
	;   '$set_source_module'(M, M),
	    N = M:N0
	),
	push_op(P, T, N),
	assert_op(Src, op(P,T,N)),
	debug(xref, ':- ~w.', [op(P,T,N)]).


%	xref_clean(+Src)
%	
%	Reset the database for the given source.

xref_clean(Source) :-
	canonical_source(Source, Src),
	retractall(called(_, Src, _Origin)),
	retractall(dynamic(_, Src)),
	retractall(multifile(_, Src)),
	retractall(defined(_, Src, _Line)),
	retractall(foreign(_, Src, _Line)),
	retractall(constraint(_, Src, _Line)),
	retractall(imported(_, Src, _From)),
	retractall(exported(_, Src)),
	retractall(xmodule(_, Src)),
	retractall(xop(Src, _)),
	retractall(source(Src, _)),
	retractall(used_class(_, Src)),
	retractall(defined_class(_, _, _, Src, _)),
	retractall(mode(_, Src)).
	

		 /*******************************
		 *	    READ RESULTS	*
		 *******************************/

%	xref_current_source(?Source)
%
%	Check what sources have been analysed.

xref_current_source(Source) :-
	source(Source, _Time).


%	xref_done(+Source, -Time)
%	
%	Cross-reference executed at Time

xref_done(Source, Time) :-
	canonical_source(Source, Src),
	source(Src, Time).


%	xref_called(+Source, ?Called, ?By)
%	
%	Enumerate the predicate-call relations. Predicate called by
%	directives have a By '<directive>'.

xref_called(Source, Called, By) :-
	canonical_source(Source, Src),
	called(Called, Src, By).

%	xref_called(+Source, ?Called)
%
%	Called is called by 

xref_called(Source, Called) :-
	canonical_source(Source, Src),
	called(Called, Src, By),
	By \= Called.			% delete recursive calls


%	xref_defined(+Source, +Goal, ?How)
%	
%	Test if Goal is accessible in Source. If this is the case, How
%	specifies the reason why the predicate is accessible. Note that
%	this predicate does not deal with built-in or global predicates,
%	just locally defined and imported ones.

xref_defined(Source, Called, How) :-
	canonical_source(Source, Src),
	xref_defined2(How, Src, Called).

xref_defined2((dynamic), Src, Called) :-
	dynamic(Called, Src).
xref_defined2((thread_local), Src, Called) :-
	thread_local(Called, Src).
xref_defined2((multifile), Src, Called) :-
	multifile(Called, Src).
xref_defined2(local(Line), Src, Called) :-
	defined(Called, Src, Line).
xref_defined2(foreign(Line), Src, Called) :-
	foreign(Called, Src, Line).
xref_defined2(constraint(Line), Src, Called) :-
	constraint(Called, Src, Line).
xref_defined2(imported(From), Src, Called) :-
	imported(Called, Src, From).

xref_exported(Source, Called) :-
	canonical_source(Source, Src),
	exported(Called, Src).

%	xref_module(?Source, ?Module)
%	
%	Module(s) defined in Source.

xref_module(Source, Module) :-
	canonical_source(Source, Src),
	xmodule(Module, Src).

%	xref_op(?Source, ?op(P,T,N))
%	
%	Give the operators active inside the module. This is intended to
%	setup the environment for incremental parsing of a term from the
%	source-file.

xref_op(Source, Op) :-
	canonical_source(Source, Src),
	xop(Src, Op).

xref_built_in(Head) :-
	system_predicate(Head).

xref_used_class(Source, Class) :-
	canonical_source(Source, Src),
	used_class(Class, Src).

xref_defined_class(Source, Class, local(Line, Super, Summary)) :-
	canonical_source(Source, Src),
	defined_class(Class, Super, Summary, Src, Line),
	integer(Line), !.
xref_defined_class(Source, Class, file(File)) :-
	canonical_source(Source, Src),
	defined_class(Class, _, _, Src, file(File)).

collect(Src) :-
	open_source(Src, Fd),		% also skips #! line if present
	repeat,
	    '$set_source_module'(SM, SM),
	    catch(read_term(Fd, Term,
			    [ term_position(TermPos),
			      module(SM)
			    ]), E, syntax_error(E)),
	    xref_expand(Term, T),
	    (   T == end_of_file
	    ->  !,
	        close(Fd)
	    ;   arg(2, TermPos, Line),
		flag(xref_src_line, _, Line),
	        process(T, Src),
		fail
	    ).

syntax_error(E) :-
	(   verbose
	->  print_message(error, E)
	;   true
	),
	fail.


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%	xref_expand(+Term, -Expanded)
%
%	Do the term-expansion. We have to pass require as we need it for
%	validation. Otherwise we do term-expansion,  handling all of the
%	XPCE class compiler as normal   Prolog  afterwards. CHR programs
%	are processed using process_chr/2  directly   from  the  source,
%	which is why we inhibit expansion here.

xref_expand((:- require(X)),
	    (:- require(X))) :- !.
xref_expand(Term, _) :-
	requires_library(Term, Lib),
	ensure_loaded(user:Lib),
	fail.
xref_expand(Term, Term) :-
	chr_expandable(Term), !.
xref_expand('$:-'(X), '$:-'(X)) :- !,	% boot module
	style_check(+dollar).
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
process((?- Directive), Src) :- !,
	process_directive(Directive, Src), !.
process((Head :- Body), Src) :- !,
	assert_defined(Src, Head),
	process_body(Body, Head, Src).
process('$source_location'(_File, _Line):Clause, Src) :- !,
	process(Clause, Src).
process(Term, Src) :-
	chr_expandable(Term), !,
	process_chr(Term, Src).
process(Head, Src) :-
	assert_defined(Src, Head).

		/********************************
		 *           DIRECTIVES		*
		 ********************************/

process_directive(Var, _) :-
	var(Var), !.			% error, but that isn't our business
process_directive((A,B), Src) :- !,	% TBD: whta about other control
	process_directive(A, Src),	% structures?
	process_directive(B, Src).
process_directive(List, Src) :-
	is_list(List), !,
	process_directive(consult(List), Src).
process_directive(use_module(Spec, Import), Src) :-
	xref_source_file(Spec, Path, Src),
	assert_import(Src, Import, Path).
process_directive(use_module(Modules), Src) :-
	process_use_module(Modules, Src).
process_directive(consult(Modules), Src) :-
	process_use_module(Modules, Src).
process_directive(ensure_loaded(Modules), Src) :-
	process_use_module(Modules, Src).
process_directive(load_files(Files, _Options), Src) :-
	process_use_module(Files, Src).
process_directive(include(Files), Src) :-
	process_include(Files, Src).
process_directive(dynamic(Dynamic), Src) :-
	assert_dynamic(Src, Dynamic).
process_directive(thread_local(Dynamic), Src) :-
	assert_thread_local(Src, Dynamic).
process_directive(multifile(Dynamic), Src) :-
	assert_multifile(Src, Dynamic).
process_directive(module(Module, Export), Src) :-
	assert_module(Src, Module),
	assert_export(Src, Export).
process_directive(pce_begin_class_definition(Name, Meta, Super, Doc), Src) :-
	assert_defined_class(Src, Name, Meta, Super, Doc).
process_directive(pce_autoload(Name, From), Src) :-
	assert_defined_class(Src, Name, imported_from(From)).

process_directive(op(P, A, N), Src) :-
	xref_push_op(Src, P, A, N).
process_directive(style_check(X), _) :-
	style_check(X).
process_directive(system_module, _) :-
	style_check(+dollar).
process_directive(set_prolog_flag(character_escapes, Esc), _) :-
	set_prolog_flag(character_escapes, Esc).
process_directive(pce_expansion:push_compile_operators, _) :-
	'$set_source_module'(SM, SM),
	pce_expansion:push_compile_operators(SM).
process_directive(pce_expansion:pop_compile_operators, _) :-
	pce_expansion:pop_compile_operators.
process_directive(meta_predicate(Meta), _) :-
	process_meta_predicate(Meta).
process_directive(arithmetic_function(Name/Arity), Src) :-
	PredArity is Arity + 1,
	functor(Goal, Name, PredArity),
	flag(xref_src_line, Line, Line),
	assert_called(Src, '<directive>'(Line), Goal).
process_directive(Goal, Src) :-
	flag(xref_src_line, Line, Line),
	process_body(Goal, '<directive>'(Line), Src).

%	process_meta_predicate(+Decl)
%	
%	Create prolog:meta_goal/2 declaration from the meta-goal
%	declaration.

process_meta_predicate((A,B)) :- !,
	process_meta_predicate(A),
	process_meta_predicate(B).
process_meta_predicate(Decl) :-
	functor(Decl, Name, Arity),
	functor(Head, Name, Arity),
	meta_args(1, Arity, Decl, Head, Meta),
	(   (   prolog:meta_goal(Head, _)
	    ;   prolog:called_by(Head, _)
	    ;   meta_goal(Head, _)
	    )
	->  true
	;   assert(meta_goal(Head, Meta))
	).

meta_args(I, Arity, _, _, []) :-
	I > Arity, !.
meta_args(I, Arity, Decl, Head, [H|T]) :-
	arg(I, Decl, :), !,
	arg(I, Head, H),
	I2 is I + 1,
	meta_args(I2, Arity, Decl, Head, T).
meta_args(I, Arity, Decl, Head, Meta) :-
	I2 is I + 1,
	meta_args(I2, Arity, Decl, Head, Meta).


	      /********************************
	      *             BODY	      *
	      ********************************/

xref_meta((A, B), 		[A, B]).
xref_meta((A; B), 		[A, B]).
xref_meta((A| B), 		[A, B]).
xref_meta((A -> B),		[A, B]).
xref_meta((A *-> B),		[A, B]).
xref_meta(findall(_V, G, _L),	[G]).
xref_meta(setof(_V, G, _L),	[G]).
xref_meta(bagof(_V, G, _L),	[G]).
xref_meta(forall(A, B),		[A, B]).
xref_meta(maplist(G, _),	[G+1]).
xref_meta(maplist(G, _, _),	[G+2]).
xref_meta(maplist(G, _, _, _),	[G+3]).
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
xref_meta(retract(Rule),	[G]) :- head_of(Rule, G).
xref_meta(clause(G, _),		[G]).
xref_meta(clause(G, _, _),	[G]).
xref_meta(phrase(G, _A),	[G+2]).
xref_meta(phrase(G, _A, _R),	[G+2]).
xref_meta(catch(A, _, B),	[A, B]).
xref_meta(thread_create(A,_,_), [A]).
xref_meta(thread_at_exit(A),	[A]).
xref_meta(predsort(A,_,_),	[A+3]).
xref_meta(call_cleanup(A, B),	[A, B]).
xref_meta(call_cleanup(A, _, B),[A, B]).
xref_meta(on_signal(_,_,A),	[A+1]).
xref_meta(with_mutex(_,A),	[A]).
xref_meta(assume(G),		[G]).	% library(debug)
xref_meta(assertion(G),		[G]).	% library(debug)
xref_meta(freeze(_, G),		[G]).
xref_meta(when(C, A),		[C, A]).
xref_meta(clause(G, _),		[G]).
xref_meta(clause(G, _, _),	[G]).
xref_meta(time(G),		[G]).	% development system
xref_meta(profile(G),		[G]).
xref_meta(at_halt(G),		[G]).
xref_meta(call_with_time_limit(_, G), [G]).
xref_meta(call_with_depth_limit(G, _, _), [G]).

					% XPCE meta-predicates
xref_meta(pce_global(_, new(_)), _) :- !, fail.
xref_meta(pce_global(_, B),     [B+1]).
xref_meta(ifmaintainer(G),	[G]).	% used in manual
xref_meta(listen(_, G),		[G]).	% library(broadcast)
xref_meta(listen(_, _, G),	[G]).
xref_meta(in_pce_thread(G),	[G]).

xref_meta(G, Meta) :-			% call user extensions
	prolog:meta_goal(G, Meta).
xref_meta(G, Meta) :-			% Generated from :- meta_predicate
	meta_goal(G, Meta).


%	head_of(+Rule, -Head)
%	
%	Get the head for a retract call.

head_of(Var, _) :-
	var(Var), !, fail.
head_of((Head :- _), Head).
head_of(Head, Head).

%	xref_hook(?Callable)
%	
%	Definition of known hooks.  Hooks  that   can  be  called in any
%	module are unqualified.  Other  hooks   are  qualified  with the
%	module where they are called.

xref_hook(term_expansion(_,_)).
xref_hook(goal_expansion(_,_)).
xref_hook(resource(_,_,_)).
xref_hook(prolog:message(_,_,_)).
xref_hook(user:portray(_)).
xref_hook(user:file_search_path(_,_)).
xref_hook(user:library_directory(_)).
xref_hook(user:message_hook(_,_,_)).
xref_hook(user:prolog_list_goal(_)).
xref_hook(user:prolog_trace_interception(_,_,_,_)).
xref_hook(user:prolog_clause_name(_,_)).
xref_hook(user:prolog_predicate_name(_,_)).
xref_hook(shlib:unload_all_foreign_libraries).


%	process_body(+Body, +Origin, +Src)
%	
%	Process a callable body (body of a clause or directive). Origin
%	describes the origin of the call.

process_body(Var, _, _) :-
	var(Var), !.
process_body(Goal, Origin, Src) :-
	prolog:called_by(Goal, Called), !,
	assert_called(Src, Origin, Goal),
	process_called_list(Called, Origin, Src).
process_body(Goal, Origin, Src) :-
	process_xpce_goal(Goal, Origin, Src), !.
process_body(load_foreign_library(File), _Origin, Src) :-
	process_foreign(File, Src).
process_body(load_foreign_library(File, _Init), _Origin, Src) :-
	process_foreign(File, Src).
process_body(Goal, Origin, Src) :-
	xref_meta(Goal, Metas), !,
	assert_called(Src, Origin, Goal),
	process_called_list(Metas, Origin, Src).
process_body(Goal, Origin, Src) :-
	asserting_goal(Goal, Rule), !,
	assert_called(Src, Origin, Goal),
	process_assert(Rule, Origin, Src).
process_body(Goal, Origin, Src) :-
	assert_called(Src, Origin, Goal).

process_called_list([], _, _).
process_called_list([H|T], Origin, Src) :-
	process_meta(H, Origin, Src),
	process_called_list(T, Origin, Src).

process_meta(A+N, Origin, Src) :- !,
	(   callable(A),
	    \+ A = _:_
	->  A =.. List,
	    length(Rest, N),
	    append(List, Rest, NList),
	    Term =.. NList,
	    process_body(Term, Origin, Src)
	;   true
	).
process_meta(G, Origin, Src) :-
	process_body(G, Origin, Src).

asserting_goal(assert(Rule), Rule).
asserting_goal(asserta(Rule), Rule).
asserting_goal(assertz(Rule), Rule).
asserting_goal(assert(Rule,_), Rule).
asserting_goal(asserta(Rule,_), Rule).
asserting_goal(assertz(Rule,_), Rule).

process_assert(0, _, _) :- !.		% catch variables
process_assert(_:-Body, Origin, Src) :- !,
	process_body(Body, Origin, Src).
process_assert(_, _, _).


		 /*******************************
		 *	    XPCE STUFF		*
		 *******************************/

pce_goal(new(_,_), new(-, new)).
pce_goal(send(_,_), send(arg, msg)).
pce_goal(send_class(_,_,_), send_class(arg, arg, msg)).
pce_goal(get(_,_,_), get(arg, msg, -)).
pce_goal(get_class(_,_,_), get_class(arg, arg, msg, -)).
pce_goal(get_chain(_,_,_), get_chain(arg, msg, -)).
pce_goal(get_object(_,_,_), get_object(arg, msg, -)).

process_xpce_goal(G, Origin, Src) :-
	pce_goal(G, Process), !,
	assert_called(Src, Origin, G),
	(   arg(I, Process, How),
	    arg(I, G, Term),
	    process_xpce_arg(How, Term, Origin, Src),
	    fail
	;   true
	).
		
process_xpce_arg(new, Term, Origin, Src) :-
	callable(Term),
	process_new(Term, Origin, Src).
process_xpce_arg(arg, Term, Origin, Src) :-
	compound(Term),
	process_new(Term, Origin, Src).
process_xpce_arg(msg, Term, Origin, Src) :-
	compound(Term),
	(   arg(_, Term, Arg),
	    process_xpce_arg(arg, Arg, Origin, Src),
	    fail
	;   true
	).

	
process_new(Term, Origin, Src) :-
	assert_new(Src, Origin, Term),
	(   arg(_, Term, Arg),
	    process_xpce_arg(arg, Arg, Origin, Src),
	    fail
	;   true
	).

assert_new(Src, Origin, Term) :-
	compound(Term),
	arg(1, Term, Prolog),
	Prolog == @prolog,
	(   Term =.. [message, _, Selector | T],
	    atom(Selector)
	->  Called =.. [Selector|T],
	    process_body(Called, Origin, Src)
	;   Term =.. [?, _, Selector | T],
	    atom(Selector)
	->  append(T, [_R], T2),
	    Called =.. [Selector|T2],
	    process_body(Called, Origin, Src)
	),
	fail.
assert_new(_, _, @_) :- !.
assert_new(Src, _, Term) :-
	callable(Term),
	functor(Term, Name, _),
	assert_used_class(Src, Name).


		/********************************
		*       INCLUDED MODULES	*
		********************************/

process_use_module(_Module:_Files, _) :- !.	% loaded in another module
process_use_module([], _) :- !.
process_use_module([H|T], Src) :- !,
	process_use_module(H, Src),
	process_use_module(T, Src).
process_use_module(library(pce), Src) :- !,	% bit special
	xref_public_list(library(pce), Path, Public, Src),
	forall(member(Import, Public),
	       process_pce_import(Import, Src, Path)).
process_use_module(File, Src) :-
	(   catch(xref_public_list(File, Path, Public, Src), _, fail)
	->  assert_import(Src, Public, Path),
	    (	File = library(chr)	% hacky
	    ->	assert(mode(chr, Src))
	    ;	true
	    )
	;   true
	).

process_pce_import(Name/Arity, Src, Path) :-
	atom(Name),
	integer(Arity), !,
	functor(Term, Name, Arity),
	(   \+ system_predicate(Term),
	    \+ Term = pce_error(_) 	% hack!?
	->  assert_import(Src, Name/Arity, Path)
	;   true
	).
process_pce_import(op(P,T,N), Src, _) :-
	xref_push_op(Src, P, T, N).

%	xref_public_list(+File, -Path, -Public, +Src)
%	
%	Find File as referenced from Src. Unify Path with the an
%	absolute path to the referenced source and Public with a
%	Name/Arity list holding all the public predicates exported from
%	that (module) file.

xref_public_list(File, Path, Public, Src) :-
	xref_source_file(File, Path, Src),
	open_source(Path, Fd),		% skips possible #! line
	call_cleanup(read(Fd, ModuleDecl), close(Fd)),
	ModuleDecl = (:- module(_, Public)).


		 /*******************************
		 *	       INCLUDE		*
		 *******************************/

process_include([], _) :- !.
process_include([H|T], Src) :- !,
	process_include(H, Src),
	process_include(T, Src).
process_include(File, Src) :-
	catch(read_src_to_terms(File, Src, Terms), _, fail), !,
	process_terms(Terms, Src).
process_include(_, _).

process_terms([], _).
process_terms([H|T], Src) :-
	process(H, Src),
	process_terms(T, Src).

read_src_to_terms(File, Src, Terms) :-
	xref_source_file(File, Path, Src),
	open_source(Path, Fd),
	call_cleanup(read_clauses(Fd, Terms),
		     close(Fd)).
	
read_clauses(In, Terms) :-
	read_clause(In, C0),
	read_clauses(C0, In, Terms).

read_clauses(end_of_file, _, []) :- !.
read_clauses(Term, In, [Term|T]) :-
	read_clause(In, C),
	read_clauses(C, In, T).


%	process_foreign(+Spec, +Src)
%	
%	Process a load_foreign_library/1 call.

process_foreign(Spec, Src) :-
	current_foreign_library(Spec, Defined),
	(   xmodule(Module, Src)
	->  true
	;   Module = user
	),
	process_foreign_defined(Defined, Module, Src).

process_foreign_defined([], _, _).
process_foreign_defined([H|T], M, Src) :-
	(   H = M:Head
	->  assert_foreign(Src, Head)
	;   assert_foreign(Src, H)
	),
	process_foreign_defined(T, M, Src).


		 /*******************************
		 *	    CHR SUPPORT		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This part of the file supports CHR. Our choice is between making special
hooks to make CHR expansion work and  then handle the (complex) expanded
code or process the  CHR  source   directly.  The  latter looks simpler,
though I don't like the idea  of   adding  support for libraries to this
module.  A  file  is  supposed  to  be  a    CHR   file  if  it  uses  a
use_module(library(chr) or contains a :-   constraint/1 directive. As an
extra bonus we get the source-locations right :-)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

chr_expandable((:- constraints(_))).
chr_expandable((constraints(_))).
chr_expandable((handler(_))) :-
	is_chr_file.
chr_expandable((rules(_))) :-
	is_chr_file.
chr_expandable(<=>(_, _)) :-
	is_chr_file.
chr_expandable(@(_, _)) :-
	is_chr_file.
chr_expandable(==>(_, _)) :-
	is_chr_file.
chr_expandable(pragma(_, _)) :-
	is_chr_file.
chr_expandable(option(_, _)) :-
	is_chr_file.

is_chr_file :-
	source(Src, _),
	mode(chr, Src), !.

process_chr(@(_Name, Rule), Src) :-
	process_chr(Rule, Src).
process_chr(pragma(Rule, _Pragma), Src) :-
	process_chr(Rule, Src).
process_chr(<=>(Head, Body), Src) :-
	chr_head(Head, Src, H),
	chr_body(Body, H, Src).
process_chr(==>(Head, Body), Src) :-
	chr_head(Head, H, Src),
	chr_body(Body, H, Src).
process_chr((:- constraints(C)), Src) :-
	process_chr(constraints(C), Src).
process_chr(constraints(_), Src) :-
	(   mode(chr, Src)
	->  true
	;   assert(mode(chr, Src))
	).

chr_head(X, _, _) :-
	var(X), !.			% Illegal.  Warn?
chr_head(\(A,B), Src, H) :-
	chr_head(A, Src, H),
	process_body(B, H, Src).
chr_head((H0,B), Src, H) :-
	chr_defined(H0, Src, H),
	process_body(B, H, Src).
chr_head(H0, Src, H) :-
	chr_defined(H0, Src, H).

chr_defined(X, _, _) :-
	var(X), !.
chr_defined(#(C,_Id), Src, C) :- !,
	assert_constraint(Src, C).
chr_defined(A, Src, A) :-
	assert_constraint(Src, A).

chr_body(X, From, Src) :-
	var(X), !,
	process_body(X, From, Src).
chr_body('|'(Guard, Goals), H, Src) :- !,
	chr_body(Guard, H, Src),
	chr_body(Goals, H, Src).
chr_body(G, From, Src) :-
	process_body(G, From, Src).

assert_constraint(_, Head) :-
	var(Head), !.
assert_constraint(Src, Head) :-
	constraint(Head, Src, _), !.
assert_constraint(Src, Head) :-
	functor(Head, Name, Arity),
	functor(Term, Name, Arity),
	flag(xref_src_line, Line, Line),
	assert(constraint(Term, Src, Line)).


		/********************************
		*       PHASE 1 ASSERTIONS	*
		********************************/

%	assert_called(+Src, +From, +Head)
%
%	Assert the fact that Head is called by From in Src. We do not
%	assert called system predicates.

assert_called(_, _, Var) :-
	var(Var), !.
assert_called(Src, From, Goal) :-
	var(From), !,
	assert_called(Src, '<unknown>', Goal).
assert_called(Src, Origin, M:G) :- !,
	(   atom(M),
	    xmodule(M, Src)
	->  assert_called(Src, Origin, G)
	;   true			% call to other module
	).
assert_called(_, _, Goal) :-
	system_predicate(Goal), !.
assert_called(Src, Origin, Goal) :-
	called(Goal, Src, Origin), !.
assert_called(Src, Origin, Goal) :-
	generalise(Origin, OTerm),
	generalise(Goal, Term),
	assert(called(Term, Src, OTerm)).

assert_defined(Src, Goal) :-
	defined(Goal, Src, _), !.
assert_defined(Src, Goal) :-
	generalise(Goal, Term),
	flag(xref_src_line, Line, Line),
	assert(defined(Term, Src, Line)).

assert_foreign(Src, Goal) :-
	foreign(Goal, Src, _), !.
assert_foreign(Src, Goal) :-
	generalise(Goal, Term),
	flag(xref_src_line, Line, Line),
	assert(foreign(Term, Src, Line)).

assert_import(_, [], _) :- !.
assert_import(Src, [H|T], From) :- !,
	assert_import(Src, H, From),
	assert_import(Src, T, From).
assert_import(Src, Name/Arity, From) :- !,
	functor(Term, Name, Arity),
	assert(imported(Term, Src, From)).
assert_import(Src, op(P,T,N), _) :-
	xref_push_op(Src, P,T,N).

%	assert_op(+Src, op(P,T,N))

assert_op(Src, op(P,T,_:N)) :-
	(   xop(Src, op(P,T,N))
	->  true
	;   assert(xop(Src, op(P,T,N)))
	).

%	assert_module(+Src, +Module)
%	
%	Assert we are loading code into Module.  This is also used to
%	exploit local term-expansion and other rules.

assert_module(Src, $(Module)) :-	% deal with system modules
	atom(Module), !,
	atom_concat($, Module, Name),
	assert_module(Src, Name).
assert_module(Src, Module) :-
	xmodule(Module, Src), !.
assert_module(Src, Module) :-
	'$set_source_module'(_, Module),
	assert(xmodule(Module, Src)),
	(   sub_atom(Module, 0, _, _, $)
	->  style_check(+dollar)
	;   true
	).

assert_export(_, []) :- !.
assert_export(Src, [H|T]) :-
	assert_export(Src, H),
	assert_export(Src, T).
assert_export(Src, Name0/Arity) :-
	(   Name0 = $(Hidden)		% deal with system modules
	->  atom_concat($, Hidden, Name)
	;   Name = Name0
	),
	functor(Term, Name, Arity),
	assert(exported(Term, Src)).
assert_export(Src, op(P, A, N)) :-
	xref_push_op(Src, P, A, N).

assert_dynamic(Src, (A, B)) :- !,
	assert_dynamic(Src, A),
	assert_dynamic(Src, B).
assert_dynamic(_, _M:_Name/_Arity) :- !. % not local
assert_dynamic(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	(   thread_local(Term, Src)	% dynamic after thread_local has
	->  true			% no effect
	;   assert(dynamic(Term, Src))
	).

assert_thread_local(Src, (A, B)) :- !,
	assert_thread_local(Src, A),
	assert_thread_local(Src, B).
assert_thread_local(_, _M:_Name/_Arity) :- !. % not local
assert_thread_local(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(thread_local(Term, Src)).

assert_multifile(Src, (A, B)) :- !,
	assert_multifile(Src, A),
	assert_multifile(Src, B).
assert_multifile(_, _M:_Name/_Arity) :- !. % not local
assert_multifile(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	assert(multifile(Term, Src)).

assert_used_class(Src, Name) :-
	used_class(Name, Src), !.
assert_used_class(Src, Name) :-
	assert(used_class(Name, Src)).

assert_defined_class(Src, Name, _Meta, _Super, _) :-
	defined_class(Name, _, _, Src, _), !.
assert_defined_class(_, _, _, -, _) :- !. 		% :- pce_extend_class
assert_defined_class(Src, Name, Meta, Super, Summary) :-
	flag(xref_src_line, Line, Line),
	(   Summary == @default
	->  Atom = ''
	;   is_list(Summary)
	->  atom_codes(Atom, Summary)
	;   string(Summary)
	->  atom_concat(Summary, '', Atom)
	),
	assert(defined_class(Name, Super, Atom, Src, Line)),
	(   Meta = @_
	->  true
	;   assert_used_class(Src, Meta)
	),
	assert_used_class(Src, Super).

assert_defined_class(Src, Name, imported_from(_File)) :-
	defined_class(Name, _, _, Src, _), !.
assert_defined_class(Src, Name, imported_from(File)) :-
	assert(defined_class(Name, _, '', Src, file(File))).


		/********************************
		*            UTILITIES		*
		********************************/

%	generalise(+Callable, -General)
%	
%	Generalise a callable term.

generalise(Var, Var) :-
	var(Var), !.			% error?
generalise(pce_principal:send_implementation(Id, _, _),
	   pce_principal:send_implementation(Id, _, _)) :-
	atom(Id), !.
generalise(pce_principal:get_implementation(Id, _, _, _),
	   pce_principal:get_implementation(Id, _, _, _)) :-
	atom(Id), !.
generalise('<directive>'(Line), '<directive>'(Line)) :- !.
generalise(Module:Goal0, Module:Goal) :-
	atom(Module), !,
	generalise(Goal0, Goal).
generalise(Term0, Term) :-
	callable(Term0),
	functor(Term0, Name, Arity),
	functor(Term, Name, Arity).


%	xref_source_file(+Spec, -File, +Src)
%	
%	Find named source file.

xref_source_file(Plain, File, Source) :-
	canonical_source(Source, Src),
	atom(Plain),
	(   integer(Src),
	    object(@Src)
	->  get(@Src?file, absolute_path, Path)
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
	(   Obj = @Src,
	    object(Obj)
	->  pce_open(Obj, read, Fd)
	;   open(Src, read, Fd)
	),
	(   peek_char(Fd, #)		% Deal with #! script
	->  skip(Fd, 10)
	;   true
	).

canonical_source(Object, Ref) :-
	object(Object), !,
	Object = @Ref.
canonical_source(Ref, Ref) :-
	integer(Ref), !.
canonical_source(User, user) :-
	User == user, !.
canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     access(read),
			     file_errors(fail)
			   ],
			   Src), !.
canonical_source(Source, Src) :-
	var(Source), !,
	Src = Source.

