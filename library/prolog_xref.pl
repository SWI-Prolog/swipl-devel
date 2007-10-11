/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(prolog_xref,
	  [ xref_source/1,		% +Source
	    xref_called/3,		% ?Source, ?Callable, ?By
	    xref_defined/3,		% ?Source. ?Callable, -How
	    xref_definition_line/2,	% +How, -Line
	    xref_exported/2,		% ?Source, ?Callable
	    xref_module/2,		% ?Source, ?Module
	    xref_op/2,			% ?Source, ?Op
	    xref_clean/1,		% +Source
	    xref_current_source/1,	% ?Source
	    xref_done/2,		% +Source, -Time
	    xref_built_in/1,		% ?Callable
	    xref_expand/2,		% +Term, -Expanded
	    xref_source_file/3,		% +Spec, -Path, +Source
	    xref_source_file/4,		% +Spec, -Path, +Source, +Options
	    xref_public_list/4,		% +Path, -Export, +Src
	    xref_meta/2,		% +Goal, -Called
	    xref_hook/1,		% ?Callable
					% XPCE class references
	    xref_used_class/2,		% ?Source, ?ClassName
	    xref_defined_class/3	% ?Source, ?ClassName, -How
	  ]).
:- use_module(library(debug), [debug/3, debugging/1]).
:- use_module(library(lists), [append/3, member/2]).
:- use_module(library(operators),
	      [pop_operators/0, push_op/3, push_operators/1]).
:- use_module(library(shlib), [current_foreign_library/2]).
:- use_module(library(prolog_source)).
:- use_module(library(option)).

:- dynamic
	called/3,			% Head, Src, From
	(dynamic)/3,			% Head, Src, Line
	(thread_local)/3,		% Head, Src, Line
	(multifile)/3,			% Head, Src, Line
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
	prolog:meta_goal/2,		% +Goal, -Pattern
	prolog:hook/1.			% +Callable

:- dynamic
	meta_goal/2.

called_by(Goal, Called) :-
	prolog:called_by(Goal, Called), !.
called_by(on_signal(_,_,New), [New+1]) :-
	(   new == throw
	;   new == default
	), !, fail.


		 /*******************************
		 *	     BUILT-INS		*
		 *******************************/

%%	built_in_predicate(+Callable)
%	
%	True if Callable is a built-in

system_predicate(Goal) :-
	predicate_property(system:Goal, built_in), !.


		/********************************
		*            TOPLEVEL		*
		********************************/

verbose :-
	debugging(xref).

%%	xref_source(+Source) is det.
%	
%	Generate the cross-reference data  for   Source  if  not already
%	done and the source is not modified.  Checking for modifications
%	is only done for files.
%	
%	@param Source	File specification or XPCE buffer

xref_source(Source) :-
	prolog_canonical_source(Source, Src),
	(   atom(Src)
	->  time_file(Src, Modified),
	    source(Src, Modified)
	), !.
xref_source(Source) :-
	prolog_canonical_source(Source, Src),
	xref_clean(Src),
	(   atom(Src)
	->  time_file(Src, Modified)
	;   get_time(Modified)		% Actually should be `generation'
	),
	assert(source(Src, Modified)),
	xref_setup(Src, In, State),
	call_cleanup(collect(Src, In), xref_cleanup(State)).

:- thread_local
	xref_stream/1.			% input stream

xref_setup(Src, In, state(In, Xref, [SRef|HRefs])) :-
	prolog_open_source(Src, In),
	asserta(xref_stream(In), SRef),
	(   current_prolog_flag(xref, Xref)
	->  true
	;   Xref = false
	),
	set_prolog_flag(xref, true),
	(   verbose
	->  HRefs = []
	;   asserta(user:message_hook(_,_,_), Ref),
	    HRefs = [Ref]
	).

xref_cleanup(state(In, Xref, Refs)) :-
	prolog_close_source(In),
	set_prolog_flag(xref, Xref),
	maplist(erase, Refs).

%%	xref_input_stream(-Stream) is det.
%
%	Current input stream for cross-referencer.

xref_input_stream(Stream) :-
	xref_stream(Var), !,
	Stream = Var.

%%	xref_push_op(Source, +Prec, +Type, :Name)
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


%%	xref_clean(+Source) is det.
%	
%	Reset the database for the given source.

xref_clean(Source) :-
	prolog_canonical_source(Source, Src),
	retractall(called(_, Src, _Origin)),
	retractall(dynamic(_, Src, Line)),
	retractall(multifile(_, Src, Line)),
	retractall(defined(_, Src, Line)),
	retractall(foreign(_, Src, Line)),
	retractall(constraint(_, Src, Line)),
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

%%	xref_current_source(?Source)
%
%	Check what sources have been analysed.

xref_current_source(Source) :-
	source(Source, _Time).


%%	xref_done(+Source, -Time) is det.
%	
%	Cross-reference executed at Time

xref_done(Source, Time) :-
	prolog_canonical_source(Source, Src),
	source(Src, Time).


%%	xref_called(+Source, ?Called, ?By) is nondet.
%	
%	Enumerate the predicate-call relations. Predicate called by
%	directives have a By '<directive>'.

xref_called(Source, Called, By) :-
	prolog_canonical_source(Source, Src),
	called(Called, Src, By).


%%	xref_defined(+Source, +Goal, ?How) is semidet.
%	
%	Test if Goal is accessible in Source. If this is the case, How
%	specifies the reason why the predicate is accessible. Note that
%	this predicate does not deal with built-in or global predicates,
%	just locally defined and imported ones.

xref_defined(Source, Called, How) :-
	prolog_canonical_source(Source, Src),
	xref_defined2(How, Src, Called).

xref_defined2(dynamic(Line), Src, Called) :-
	dynamic(Called, Src, Line).
xref_defined2(thread_local(Line), Src, Called) :-
	thread_local(Called, Src, Line).
xref_defined2(multifile(Line), Src, Called) :-
	multifile(Called, Src, Line).
xref_defined2(local(Line), Src, Called) :-
	defined(Called, Src, Line).
xref_defined2(foreign(Line), Src, Called) :-
	foreign(Called, Src, Line).
xref_defined2(constraint(Line), Src, Called) :-
	constraint(Called, Src, Line).
xref_defined2(imported(From), Src, Called) :-
	imported(Called, Src, From).


%%	xref_definition_line(+How, -Line)
%	
%	If the 3th argument of xref_defined contains line info, return
%	this in Line.

xref_definition_line(local(Line),	 Line).
xref_definition_line(dynamic(Line),	 Line).
xref_definition_line(thread_local(Line), Line).
xref_definition_line(multifile(Line),	 Line).
xref_definition_line(constraint(Line),	 Line).
xref_definition_line(foreign(Line),	 Line).


xref_exported(Source, Called) :-
	prolog_canonical_source(Source, Src),
	exported(Called, Src).

%%	xref_module(?Source, ?Module) is nondet.
%	
%	True if Module is defined in Source.

xref_module(Source, Module) :-
	prolog_canonical_source(Source, Src),
	xmodule(Module, Src).

%%	xref_op(?Source, Op) is nondet.
%	
%	Give the operators active inside the module. This is intended to
%	setup the environment for incremental parsing of a term from the
%	source-file.
%	
%	@param Op	Term of the form op(Priority, Type, Name)

xref_op(Source, Op) :-
	prolog_canonical_source(Source, Src),
	xop(Src, Op).

xref_built_in(Head) :-
	system_predicate(Head).

xref_used_class(Source, Class) :-
	prolog_canonical_source(Source, Src),
	used_class(Class, Src).

xref_defined_class(Source, Class, local(Line, Super, Summary)) :-
	prolog_canonical_source(Source, Src),
	defined_class(Class, Super, Summary, Src, Line),
	integer(Line), !.
xref_defined_class(Source, Class, file(File)) :-
	prolog_canonical_source(Source, Src),
	defined_class(Class, _, _, Src, file(File)).

collect(Src, In) :-
	repeat,
	    catch(read_source_term(In, Term, TermPos),
		  E, syntax_error(E)),
	    xref_expand(Term, T),
	    (   T == end_of_file
	    ->  !
	    ;   arg(2, TermPos, Line),
		flag(xref_src_line, _, Line),
	        process(T, Src),
		fail
	    ).

%%	read_source_term(+In:stream, -Term, -TermPos) is det.
%
%	Read next term  from  In.   The  cross-referencer  supports  the
%	comment_hook  as  also  implemented  by  the  compiler  for  the
%	documentation processor.

:- multifile
	prolog:comment_hook/3.

read_source_term(In, Term, TermPos) :-
	'$get_predicate_attribute'(prolog:comment_hook(_,_,_),
				   number_of_clauses, N),
	N > 0, !,
	'$set_source_module'(SM, SM),
	read_term(In, Term,
		  [ term_position(TermPos),
		    comments(Comments),
		    module(SM)
		  ]),
	(   catch(prolog:comment_hook(Comments, TermPos, Term), E,
		  print_message(error, E))
	->  true
	;   true
	).
read_source_term(In, Term, TermPos) :-
	'$set_source_module'(SM, SM),
	read_term(In, Term,
		  [ term_position(TermPos),
		    module(SM)
		  ]).


syntax_error(E) :-
	(   verbose
	->  print_message(error, E)
	;   true
	),
	fail.


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%%	xref_expand(+Term, -Expanded)
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


%%	requires_library(+Term, -Library)
%
%	known expansion hooks.  Should be more dynamic!

requires_library((:- emacs_begin_mode(_,_,_,_,_)), library(emacs_extend)).
requires_library((:- draw_begin_shape(_,_,_,_)), library(pcedraw)).


		 /*******************************
		 *	     PROCESS		*
		 *******************************/

process(Var, _) :-
	var(Var), !.			% Warn?
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
process(M:(Head :- Body), Src) :- !,
	process(M:Head :- M:Body, Src).
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
	xref_public_list(Spec, Path, Public, Src),
	assert_import(Src, Import, Public, Path).
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
process_directive(system_mode(on), _Src) :- !,
	style_check(+dollar).
process_directive(pce_begin_class_definition(Name, Meta, Super, Doc), Src) :-
	assert_defined_class(Src, Name, Meta, Super, Doc).
process_directive(pce_autoload(Name, From), Src) :-
	assert_defined_class(Src, Name, imported_from(From)).

process_directive(op(P, A, N), Src) :-
	xref_push_op(Src, P, A, N).
process_directive(style_check(X), _) :-
	style_check(X).
process_directive(encoding(Enc), _) :-
	(   xref_input_stream(Stream)
	->  set_stream(Stream, encoding(Enc))
	;   true			% can this happen?
	).
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
process_directive(arithmetic_function(FSpec), Src) :-
	arith_callable(FSpec, Goal), !,
	flag(xref_src_line, Line, Line),
	assert_called(Src, '<directive>'(Line), Goal).
process_directive(format_predicate(_, Goal), Src) :- !,
	flag(xref_src_line, Line, Line),
	assert_called(Src, '<directive>'(Line), Goal).
process_directive(Goal, Src) :-
	flag(xref_src_line, Line, Line),
	process_body(Goal, '<directive>'(Line), Src).

%%	process_meta_predicate(+Decl)
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
	    ;   called_by(Head, _)
	    ;   meta_goal(Head, _)
	    )
	->  true
	;   assert(meta_goal(Head, Meta))
	).

meta_args(I, Arity, _, _, []) :-
	I > Arity, !.
meta_args(I, Arity, Decl, Head, [H|T]) :- 		% :
	arg(I, Decl, :), !,
	arg(I, Head, H),
	I2 is I + 1,
	meta_args(I2, Arity, Decl, Head, T).
meta_args(I, Arity, Decl, Head, [H+A|T]) :-		% I --> H+I
	arg(I, Decl, A), 
	integer(A), A > 0, !,
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
xref_meta(findall(_V,G,_L),	[G]).
xref_meta(findall(_V,G,_L,_T),	[G]).
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
xref_meta(thread_signal(_,A),   [A]).
xref_meta(thread_at_exit(A),	[A]).
xref_meta(predsort(A,_,_),	[A+3]).
xref_meta(call_cleanup(A, B),	[A, B]).
xref_meta(call_cleanup(A, _, B),[A, B]).
xref_meta(setup_and_call_cleanup(A, B, C),[A, B, C]).
xref_meta(setup_and_call_cleanup(A, B, _, C),[A, B, C]).
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
xref_meta('$add_directive_wic'(G), [G]).
xref_meta(with_output_to(_, G),	[G]).

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


%%	head_of(+Rule, -Head)
%	
%	Get the head for a retract call.

head_of(Var, _) :-
	var(Var), !, fail.
head_of((Head :- _), Head).
head_of(Head, Head).

%%	xref_hook(?Callable)
%	
%	Definition of known hooks.  Hooks  that   can  be  called in any
%	module are unqualified.  Other  hooks   are  qualified  with the
%	module where they are called.

xref_hook(Hook) :-
	prolog:hook(Hook).
xref_hook(Hook) :-
	hook(Hook).


hook(attr_portray_hook(_,_)).
hook(attr_unify_hook(_,_)).
hook(goal_expansion(_,_)).
hook(term_expansion(_,_)).
hook(resource(_,_,_)).

hook(emacs_prolog_colours:goal_classification(_,_)).
hook(emacs_prolog_colours:term_colours(_,_)).
hook(emacs_prolog_colours:goal_colours(_,_)).
hook(emacs_prolog_colours:style(_,_)).
hook(emacs_prolog_colours:identify(_,_)).
hook(pce_principal:pce_class(_,_,_,_,_,_)).
hook(pce_principal:send_implementation(_,_,_)).
hook(pce_principal:get_implementation(_,_,_,_)).
hook(pce_principal:pce_lazy_get_method(_,_,_)).
hook(pce_principal:pce_lazy_send_method(_,_,_)).
hook(pce_principal:pce_uses_template(_,_)).
hook(prolog:locate_clauses(_,_)).
hook(prolog:message(_,_,_)).
hook(prolog:message_context(_,_,_)).
hook(prolog:debug_control_hook(_)).
hook(prolog:help_hook(_)).
hook(prolog:show_profile_hook(_,_)).
hook(prolog:general_exception(_,_)).
hook(prolog_edit:load).
hook(prolog_edit:locate(_,_,_)).
hook(shlib:unload_all_foreign_libraries).
hook(system:'$foreign_registered'(_, _)).
hook(user:exception(_,_,_)).
hook(user:file_search_path(_,_)).
hook(user:library_directory(_)).
hook(user:message_hook(_,_,_)).
hook(user:portray(_)).
hook(user:prolog_clause_name(_,_)).
hook(user:prolog_list_goal(_)).
hook(user:prolog_predicate_name(_,_)).
hook(user:prolog_trace_interception(_,_,_,_)).
hook(user:prolog_event_hook(_)).
hook(user:prolog_exception_hook(_,_,_,_)).


%%	arith_callable(+Spec, -Callable)
%	
%	Translate argument of arithmetic_function/1 into a callable term

arith_callable(Var, _) :-
	var(Var), !, fail.
arith_callable(Module:Spec, Module:Goal) :- !,
	arith_callable(Spec, Goal).
arith_callable(Name/Arity, Goal) :-
	PredArity is Arity + 1,
	functor(Goal, Name, PredArity).


%%	process_body(+Body, +Origin, +Src)
%	
%	Process a callable body (body of a clause or directive). Origin
%	describes the origin of the call.

process_body(Var, _, _) :-
	var(Var), !.
process_body(Goal, Origin, Src) :-
	called_by(Goal, Called), !,
	(   is_list(Called)
	->  true
	;   throw(error(type_error(list, Called), _))
	),
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
	(   extend(A, N, AX)
	->  process_body(AX, Origin, Src)
	;   true
	).
process_meta(G, Origin, Src) :-
	process_body(G, Origin, Src).

extend(Var, _, _) :-
	var(Var), !, fail.
extend(M:G, N, M:GX) :- !,
	callable(G),
	extend(G, N, GX).
extend(G, N, GX) :-
	callable(G),
	G =.. List,
	length(Rest, N),
	append(List, Rest, NList),
	GX =.. NList.

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
pce_goal(get_class(_,_,_,_), get_class(arg, arg, msg, -)).
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

process_new(_M:_Term, _, _) :- !.	% TBD: Calls on other modules!
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

%%	xref_public_list(+File, -Path, -Public, +Src)
%	
%	Find File as referenced from Src. Unify Path with the an
%	absolute path to the referenced source and Public with a
%	Name/Arity list holding all the public predicates exported from
%	that (module) file.

xref_public_list(File, Path, Public, Src) :-
	xref_source_file(File, Path, Src),
	prolog_open_source(Path, Fd),		% skips possible #! line
	call_cleanup(read(Fd, ModuleDecl), prolog_close_source(Fd)),
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
	prolog_open_source(Path, Fd),
	call_cleanup(read_clauses(Fd, Terms),
		     prolog_close_source(Fd)).
	
read_clauses(In, Terms) :-
	read_clause(In, C0),
	read_clauses(C0, In, Terms).

read_clauses(end_of_file, _, []) :- !.
read_clauses(Term, In, [Term|T]) :-
	read_clause(In, C),
	read_clauses(C, In, T).


%%	process_foreign(+Spec, +Src)
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

%%	assert_called(+Src, +From, +Head)
%
%	Assert the fact that Head is called by From in Src. We do not
%	assert called system predicates.

assert_called(_, _, Var) :-
	var(Var), !.
assert_called(Src, From, Goal) :-
	var(From), !,
	assert_called(Src, '<unknown>', Goal).
assert_called(_, _, Goal) :-
	hide_called(Goal), !.
assert_called(Src, Origin, M:G) :- !,
	(   atom(M),
	    callable(G)
	->  (   xmodule(M, Src)
	    ->  assert_called(Src, Origin, G)
	    ;   called(M:G, Src, Origin)
	    ->  true
	    ;   generalise(Origin, OTerm),
		generalise(G, GTerm),
		assert(called(M:GTerm, Src, OTerm))
	    )
	;   true                        % call to variable module
	).
assert_called(_, _, Goal) :-
	system_predicate(Goal), !.
assert_called(Src, Origin, Goal) :-
	called(Goal, Src, Origin), !.
assert_called(Src, Origin, Goal) :-
	generalise(Origin, OTerm),
	generalise(Goal, Term),
	assert(called(Term, Src, OTerm)).

%%	hide_called(:Callable)
%	
%	Goals that should not turn up as being called. Hack. Eventually
%	we should deal with that using an XPCE plugin.

hide_called(pce_principal:send_implementation(_, _, _)).
hide_called(pce_principal:get_implementation(_, _, _, _)).
hide_called(pce_principal:pce_lazy_get_method(_,_,_)).
hide_called(pce_principal:pce_lazy_send_method(_,_,_)).

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

%%	assert_import(+Src, +ImportList, +From) is det.
%%	assert_import(+Src, +ImportList, +PublicList, +From) is det.

assert_import(Src, Import, From) :-
	assert_import(Src, Import, _, From).

assert_import(_, [], _, _) :- !.
assert_import(Src, [H|T], Public, From) :- !,
	assert_import(Src, H, Public, From),
	assert_import(Src, T, Public, From).
assert_import(Src, Name/Arity, Public, From) :-
	atom(Name), integer(Arity), !,
	functor(Term, Name, Arity),
	(   member(Name/Arity, Public)
	->  assert(imported(Term, Src, From))
	;   flag(xref_src_line, Line, Line),
	    assert_called(Src, '<directive>'(Line), Term)
	).
assert_import(Src, op(P,T,N), _, _) :-
	xref_push_op(Src, P,T,N).

%%	assert_op(+Src, +Op) is det.
%
%	@param Op	Ground term op(Priority, Type, Name).

assert_op(Src, op(P,T,_:N)) :-
	(   xop(Src, op(P,T,N))
	->  true
	;   assert(xop(Src, op(P,T,N)))
	).

%%	assert_module(+Src, +Module)
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
	(   thread_local(Term, Src, _)	% dynamic after thread_local has
	->  true			% no effect
	;   flag(xref_src_line, Line, Line),
	    assert(dynamic(Term, Src, Line))
	).

assert_thread_local(Src, (A, B)) :- !,
	assert_thread_local(Src, A),
	assert_thread_local(Src, B).
assert_thread_local(_, _M:_Name/_Arity) :- !. % not local
assert_thread_local(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	flag(xref_src_line, Line, Line),
	assert(thread_local(Term, Src, Line)).

assert_multifile(Src, (A, B)) :- !,
	assert_multifile(Src, A),
	assert_multifile(Src, B).
assert_multifile(_, _M:_Name/_Arity) :- !. % not local
assert_multifile(Src, Name/Arity) :-
	functor(Term, Name, Arity),
	flag(xref_src_line, Line, Line),
	assert(multifile(Term, Src, Line)).

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

%%	generalise(+Callable, -General)
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


		 /*******************************
		 *	SOURCE MANAGEMENT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This section of the file contains   hookable  predicates to reason about
sources. The built-in code here  can  only   deal  with  files. The XPCE
library(pce_prolog_xref) provides hooks to deal with XPCE objects, so we
can do cross-referencing on PceEmacs edit   buffers.  Other examples for
hooking can be databases, (HTTP) URIs, etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	prolog:xref_source_directory/2.		% +Source, -Dir


%%	xref_source_file(+Spec, -File, +Src) is semidet.
%%	xref_source_file(+Spec, -File, +Src, +Options) is semidet.
%	
%	Find named source file from Spec, relative to Src.

xref_source_file(Plain, File, Source) :-
	xref_source_file(Plain, File, Source, []).

xref_source_file(Plain, File, Source, Options) :-
	atom(Plain),
	\+ is_absolute_file_name(Plain),
	(   prolog:xref_source_directory(Source, Dir)
	->  true
	;   atom(Source),
	    file_directory_name(Source, Dir)
	),
	concat_atom([Dir, /, Plain], Spec),
	do_xref_source_file(Spec, File, Options), !.
xref_source_file(Spec, File, _, Options) :-
	do_xref_source_file(Spec, File, Options), !.
xref_source_file(Spec, _, _, _) :-
	verbose,
	print_message(warning, error(existence_error(file, Spec), _)),
	fail.

do_xref_source_file(Spec, File, Options) :-
	option(file_type(Type), Options, prolog),
	absolute_file_name(Spec,
			   [ file_type(Type),
			     access(read),
			     file_errors(fail)
			   ], File), !.

