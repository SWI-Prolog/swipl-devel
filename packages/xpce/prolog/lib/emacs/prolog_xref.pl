/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
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

:- module(pce_xref,
	  [ xref_source/1,		% +Source
	    xref_called/2,		% ?Source, ?Callable
	    xref_called/3,		% ?Source, ?Callable, ?By
	    xref_defined/3,		% ?Source. ?Callable, -How
	    xref_exported/2,		% ?Source, ?Callable
	    xref_module/2,		% ?Source, ?Module
	    xref_clean/1,		% +Source
	    xref_current_source/1,	% ?Source
	    xref_built_in/1,		% ?Callable
	    xref_expand/2,		% +Term, -Expanded
	    xref_source_file/3,		% +Spec, -Path, +Source
	    xref_public_list/4,		% +Path, -Export, +Src
	    xref_meta/2			% +Goal, -Called
	  ]).
:- use_module(library(pce)).

:- dynamic
	called/3,			% Head, Src, From
	(dynamic)/2,			% Head, Src
	(multifile)/2,			% Head, Src
	defined/3,			% Head, Src, Line
	imported/3,			% Head, Src, From
	exported/2,			% Head, Src
	xmodule/2,			% Module, Src
	source/1.			% Src

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

:- [library('xref/common')].		% Common built-in's

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
	xref_setup(State),
	call_cleanup(collect(Src), xref_cleanup(State)).

xref_setup(state(Xref, Ref)) :-
	(   current_prolog_flag(xref, Xref)
	->  true
	;   Xref = false
	),
	set_prolog_flag(xref, true),
	asserta(user:message_hook(_,_,_), Ref).
xref_cleanup(state(Xref, Ref)) :-
	set_prolog_flag(xref, Xref),
	erase(Ref).

%	xref_clean(+Src)
%	
%	Reset the database for the given source.

xref_clean(Source) :-
	canonical_source(Source, Src),
	retractall(called(_, Src, _Origin)),
	retractall(dynamic(_, Src)),
	retractall(multifile(_, Src)),
	retractall(defined(_, Src, _Line)),
	retractall(imported(_, Src, _From)),
	retractall(exported(_, Src)),
	retractall(xmodule(_, Src)),
	retractall(source(Src)).
	

		 /*******************************
		 *	    READ RESULTS	*
		 *******************************/

%	xref_current_source(?Source)
%
%	Check what sources have been analysed.

xref_current_source(Source) :-
	source(Source).


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
xref_defined2((multifile), Src, Called) :-
	multifile(Called, Src).
xref_defined2(local(Line), Src, Called) :-
	defined(Called, Src, Line).
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

xref_expand((:- require(X)),
	    (:- require(X))) :- !.
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
	process_body(Body, Head, Src).
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
process_directive(dynamic(Dynamic), Src) :-
	assert_dynamic(Src, Dynamic).
process_directive(multifile(Dynamic), Src) :-
	assert_multifile(Src, Dynamic).
process_directive(module(Module, Export), Src) :-
	assert_module(Src, Module),
	assert_export(Src, Export).

process_directive(op(P, A, N), _) :-
	op(P, A, N).			% should be local ...
process_directive(style_check(X), _) :-
	style_check(X).			% should be local ...
process_directive(pce_expansion:push_compile_operators, _) :-
	pce_expansion:push_compile_operators.
process_directive(pce_expansion:pop_compile_operators, _) :-
	pce_expansion:pop_compile_operators.
process_directive(Goal, Src) :-
	process_body(Goal, '<directive>', Src).


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
xref_meta(with_mutex(_,A),	[A]).

					% XPCE meta-predicates
xref_meta(pce_global(_, new(_)), _) :- !, fail.
xref_meta(pce_global(_, B),     [B+1]).
xref_meta(ifmaintainer(G),	[G]).	% used in manual
xref_meta(listen(_, G),		[G]).	% library(broadcast)
xref_meta(listen(_, _, G),	[G]).

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
process_body(Goal, Origin, Src) :-
	xref_meta(Goal, Metas), !,
	assert_called(Src, Origin, Goal),
	process_called_list(Metas, Origin, Src).
process_body(Goal, Origin, Src) :-
	assert_called(Src, Origin, Goal).

process_called_list([], _, _).
process_called_list([H|T], Origin, Src) :-
	process_meta(H, Origin, Src),
	process_called_list(T, Origin, Src).

process_meta(A+N, Origin, Src) :-
	callable(A), !,
	\+ A = _:_,
	A =.. List,
	length(Rest, N),
	append(List, Rest, NList),
	Term =.. NList,
	process_body(Term, Origin, Src).
process_meta(G, Origin, Src) :-
	process_body(G, Origin, Src).


		 /*******************************
		 *	    XPCE STUFF		*
		 *******************************/

pce_goal(send(_,_)).
pce_goal(get(_,_,_)).

process_xpce_goal(G, Origin, Src) :-
	pce_goal(G), !,
	assert_called(Src, Origin, G),
	(   term_member(Term, G),
	    compound(Term),
	    arg(1, Term, Prolog),
	    Prolog == @prolog,
	    (	Term =.. [message, _, Selector | T],
		atom(Selector)
	    ->	Called =.. [Selector|T],
		process_body(Called, Origin, Src)
	    ;	Term =.. [?, _, Selector | T],
		atom(Selector)
	    ->	append(T, [_R], T2),
	        Called =.. [Selector|T2],
		process_body(Called, Origin, Src)
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
	xref_public_list(library(pce), Path, Public, Src),
	forall(member(Name/Arity, Public),
	       (   functor(Term, Name, Arity),
		   \+ system_predicate(Term),
		   \+ Term = pce_error(_) 	% hack!?
	       ->  assert_import(Src, Name/Arity, Path)
	       ;   true
	       )).
process_use_module(File, Src) :-
	(   catch(xref_public_list(File, Path, Public, Src), _, fail)
	->  assert_import(Src, Public, Path)
	;   true
	).

%	xref_public_list(+File, -Path, -Public, +Src)
%	
%	Find File as referenced from Src. Unify Path with the an
%	absolute path to the referenced source and Public with a
%	Name/Arity list holding all the public predicates exported from
%	that (module) file.

xref_public_list(File, Path, Public, Src) :-
	xref_source_file(File, Path, Src),
	open(Path, read, Fd),
	call_cleanup(read(Fd, ModuleDecl), close(Fd)),
	ModuleDecl = (:- module(_, Public)).


		/********************************
		*       PHASE 1 ASSERTIONS	*
		********************************/

%	assert_called(+Src, +From, +Head)
%
%	Assert the fact that Head is called by From in Src. We do not
%	assert called system predicates.

assert_called(_, _, Var) :-
	var(Var), !.
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
	functor(Origin, OName, OArity),
	functor(OTerm, OName, OArity),
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	assert(called(Term, Src, OTerm)).

assert_defined(_, _Module:_Head) :- !.	% defining in another module.  Bah!
assert_defined(Src, Goal) :-
	defined(Goal, Src, _), !.
assert_defined(Src, Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	flag(xref_src_line, Line, Line),
	assert(defined(Term, Src, Line)).

assert_import(_, [], _) :- !.
assert_import(Src, [H|T], From) :-
	assert_import(Src, H, From),
	assert_import(Src, T, From).
assert_import(Src, Name/Arity, From) :-
	functor(Term, Name, Arity),
	assert(imported(Term, Src, From)).

assert_module(Src, Module) :-
	xmodule(Module, Src), !.
assert_module(Src, Module) :-
	assert(xmodule(Module, Src)).

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
canonical_source(Source, Src) :-
	absolute_file_name(Source,
			   [ file_type(prolog),
			     file_errors(fail)
			   ],
			   Src).




