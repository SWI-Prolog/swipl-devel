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

:- module(prolog_xbrowse,
	  [ x_browse_analyse/1,		% +file
	    x_browse_info/2,		% +File, +Key(-Value ...) 
	    x_browse_free/1,		% +File
	    system_predicate/1,		% +Head
	    global_predicate/1		% +Head
	  ]).
:- use_module(library(pce)).
:- use_module(library('emacs/prolog_xref')).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file has a common origin   as library('emacs/prolog_xref'). I'm not
sure what to do: merge them or decide   they are different enough not to
do so and base them  on  a  common   ground.  Right  now  we import some
important things from library('emacs/prolog_xref').
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

target_prolog(native).			% The prolog system itself
%target_prolog(common).			% Common between QP 3.2 and SICStus 3

		 /*******************************
		 *	       QUERY		*
		 *******************************/

x_browse_info(Id, export(Head)) :-
	x_public(Id, Head).
x_browse_info(Id, entity(Entity, Line)) :-
	x_entity(Id, Entity, Line).


		 /*******************************
		 *	       STATE		*
		 *******************************/

:- dynamic
	current_id/1,
	current_class/1,
	called/2,
	defined/1,
	imported/1,
	x_module/1.

clean :-
	retractall(called(_, _)),
	retractall(defined(_)),
	retractall(imported(_)),
	retractall(current_id(_)),
	retractall(current_class(_)),
	retractall(x_module(_)).


		 /*******************************
		 *	     DATABASE		*
		 *******************************/

:- dynamic
	x_public/2,			% Id, Head
	x_entity/3.			% Id, Entity, Line

x_browse_free(Key) :-
	retractall(x_public(Key, _)),
	retractall(x_entity(Key, _, _)).
 
assert_entity(Entity) :-
	current_id(Key), !,
	assert_entity(Key, Entity).

assert_entity(Key, Entity) :-		% already done
	x_entity(Key, Entity, _), !.
assert_entity(Key, Entity) :-		% with source location
	source_location(_, Line), !,
	assert(x_entity(Key, Entity, Line)).
assert_entity(Key, Entity) :-		% without source
	assert(x_entity(Key, Entity, -)).

assert_predicate(M:Head) :- !,
	functor(Head, Name, Arity),
	functor(VarHead, Name, Arity),
	(   current_class(Class)
	->  assert_entity(xpce_class_local_predicate(Class, M:VarHead))
	;   assert_entity(predicate(M:VarHead))
	).
assert_predicate(Head) :-
	functor(Head, Name, Arity),
	functor(VarHead, Name, Arity),
	(   current_class(Class)
	->  assert_entity(xpce_class_local_predicate(Class, VarHead))
	;   assert_entity(predicate(VarHead))
	).

		 /*******************************
		 *	     BUILT-INS		*
		 *******************************/


:- use_module(library('xref/common')).		% Common built-in's

system_predicate(Head) :-
	target_prolog(native), !,
	predicate_property(user:Head, built_in).
system_predicate(Head) :-
	built_in(Head).

global_predicate(Head) :-
	system_predicate(Head), !.
global_predicate(Head) :-			% SWI-Prolog
	target_prolog(native),
	current_predicate(_, user:Head), !.


		/********************************
		*            TOPLEVEL		*
		********************************/

x_browse_analyse(File) :-
	clean,				% play safe
	x_browse_free(File),
	open(File, read, Fd),
	asserta(current_id(File), Ref),
	'$style_check'(Old, Old),
	style_check(+dollar),
	(   current_prolog_flag(xref, Xref)
	->  true
	;   Xref = false
	),
	set_prolog_flag(xref, true),
	repeat,
	    catch(read_term(Fd, Term,
			    [ character_escapes(true) % TBD: how to switch!?
			    ]), _, fail),
	    ignore(process_raw(Term)),
	    xref_expand(Term, T),
	    (   T == end_of_file
	    ->  !,
	        '$style_check'(_, Old),
	        close(Fd)
	    ;   process(T),
		fail
	    ),
	set_prolog_flag(xref, Xref),
	post_analysis,
	erase(Ref).


		 /*******************************
		 *	     PROCESS RAW	*
		 *******************************/

strip_method_documentation(::(Doc,Body), Doc, Body).
strip_method_documentation(Body,         '',  Body).

class_name_from_spec(_:Term, Name) :- !,
	functor(Term, Name, _).
class_name_from_spec(Term, Name) :-
	functor(Term, Name, _).

process_raw(end_of_file) :- !.
process_raw((:- module(Name, Public))) :-		% must be the first!
	current_id(Id),
	asserta(x_module(Name)),
	assert_entity(module(Name)),
	(   member(PName/Arity, Public),
	    functor(Head, PName, Arity),
	    assert(x_public(Id, Head)),
	    fail
	;   true
	).
process_raw(:->(Head, DocAndBody)) :-
	functor(Head, Name, _),
	current_class(Class),
	strip_method_documentation(DocAndBody, Doc, Body),
	assert_entity(xpce_method(send(Class, Name, Doc))),
	process_body(Body, xpce_method(send(Class, Name))).
process_raw(:<-(Head, DocAndBody)) :-
	functor(Head, Name, _),
	current_class(Class),
	strip_method_documentation(DocAndBody, Doc, Body),
	assert_entity(xpce_method(get(Class, Name, Doc))),
	process_body(Body, xpce_method(get(Class, Name))).
process_raw(variable(Name, _, _, Doc)) :-
	current_class(Class),
	assert_entity(xpce_variable(Class, Name, Doc)).
process_raw(variable(Name, Type, Access)) :-
	process_raw(variable(Name, Type, Access, '')).
process_raw(class_variable(Name, _, _, Doc)) :-
	current_class(Class),
	assert_entity(xpce_class_variable(Class, Name, Doc)).
process_raw(class_variable(Name, Type, Default)) :-
	process_raw(class_variable(Name, Type, Default, '')).
process_raw((:- PceDirective)) :-
	xpce_map(PceDirective, Mapped), !,
	process_raw((:- Mapped)).
process_raw((:- pce_begin_class(Term, Super, Doc))) :-
	class_name_from_spec(Term, Class),
	asserta(current_class(Class)),
	assert_entity(xpce_class(Class, Super, Doc)).
process_raw((:- pce_extend_class(Class))) :-
	asserta(current_class(Class)),
	assert_entity(xpce_class_extension(Class)).
process_raw((:- pce_end_class(Class))) :-
	(   current_class(Current)
	->  (   Current = Class
	    ->	retract(current_class(Class))
	    ;	print_message(error,
			      end_class_mismatch(Class, Current))
	    )
	;   print_message(error, no_class_to_end)
	).
process_raw(pce_ifhostproperty(_Cond, Terms)) :- !, % ???
	(   is_list(Terms)
	->  checklist(process_raw, Terms)
	;   process_raw(Terms)
	).
process_raw((Head --> _Body)) :-
	functor(Head, Name, Arity),
	PredArity is Arity+2,
	functor(VarHead, Name, PredArity),
	assert_entity(grammar_rule(VarHead)).
process_raw((:- Directive)) :-
	assert_entity(directive(Directive)).
process_raw((?- Directive)) :-
	assert_entity(directive(Directive)).
process_raw((M:Head :- _Body)) :-
	assert_predicate(M:Head).
process_raw((M:Head)) :-
	assert_predicate(M:Head).
process_raw((Head :- _Body)) :-
	assert_predicate(Head).
process_raw(Head) :-
	assert_predicate(Head).

xpce_map(pce_begin_class(Class, Super),
	 pce_begin_class(Class, Super, @default)).
xpce_map(pce_end_class,
	 pce_end_class(_)).
xpce_map(draw_begin_shape(Class, Super, Doc, _Event),
	 pce_begin_class(Class, Super, Doc)).
xpce_map(draw_end_shape,
	 pce_end_class(_)).
xpce_map(emacs_begin_mode(Mode, SuperMode, Doc, _, _),
	 pce_begin_class(Class, Super, Doc)) :-
	concat_atom([emacs, Mode, mode], '_', Class),
	concat_atom([emacs, SuperMode, mode], '_', Super).
xpce_map(emacs_extend_mode(Mode, _),
	 pce_extend_class(Class)) :-
	concat_atom([emacs, Mode, mode], '_', Class).
xpce_map(emacs_end_mode,
	 pce_end_class(_)).


		 /*******************************
		 *	     PROCESS		*
		 *******************************/

process(end_of_file) :- !.
process((:- Directive)) :- !,
	process_directive(Directive), !.
process((Head :- Body)) :- !,
	assert_defined(Head),
	unbind_head(Head, VarHead),
	functor(Head, Name, Arity),
	functor(VarHead, Name, Arity),
	process_body(Body, VarHead).
process('$source_location'(_File, _Line):Clause) :- !,
	process(Clause).
process(Head) :-
	assert_defined(Head).

unbind_head(X, _) :-
	var(X), !.
unbind_head(Module:Head, Module:VarHead) :- !,
	unbind_head(Head, VarHead).
unbind_head(Head, VarHead) :- !,
	functor(Head, Name, Arity),
	functor(VarHead, Name, Arity).


		/********************************
		 *           DIRECTIVES		*
		 ********************************/

process_directive(List) :-
	is_list(List), !,
	process_directive(consult(List)).
process_directive(use_module(_Module, Import)) :-
	assert_import(Import).
process_directive(require(Import)) :-	 % Include if report only
	assert_require(Import).
process_directive(use_module(Modules)) :-
	process_use_module(Modules).
process_directive(consult(Modules)) :-
	process_use_module(Modules).
process_directive(ensure_loaded(Modules)) :-
	process_use_module(Modules).
process_directive(dynamic(Dynamic)) :-
	assert_dynamic(Dynamic).

process_directive(op(P, A, N)) :-
	op(P, A, N).			% should be local ...
process_directive(Goal) :-
	process_body(Goal, (:- _)).


	      /********************************
	      *             BODY		*
	      ********************************/

%	xpce_message_goal(+Goal, -Called)
%	
%	Find calls due to message(@prolog, ...) terms.  We donot try to
%	find out whether it just concerns a data object, or is actually
%	an argument in the proper place from send, get or new.  This to
%	avoid missing cases like
%
%		Condition = message(@prolog, foobar, @arg1),
%		...
%		menu_item(foo, condition := Condition).

cascade_functor(message).
cascade_functor(?).

xpce_message_goal(Goal, SubGoal) :-
	term_member(Goal, SubTerm),
	compound(SubTerm),
	functor(SubTerm, Functor, _),
	cascade_functor(Functor),
	arg(1, SubTerm, Prolog),
	Prolog == @(prolog),
	(   (   SubTerm =.. [message, _, call | Rest]
	    ;   SubTerm =.. [message, _ | Rest]
	    )
	->  true
	;   (   SubTerm =.. [?, _, call | Rest0]
	    ;   SubTerm =.. [?, _ | Rest0]
	    )
	->  append(Rest0, [_Result], Rest)
	),
	SubGoal =.. Rest.

process_body(Goal, From) :-
	xref_meta(Goal, Metas), !,
	assert_called(Goal, From),
	process_called_list(Metas, From).
process_body(Goal, From) :-
	xpce_message_goal(Goal, SubGoal),
	process_body(SubGoal, From),
	fail.
process_body(Goal, From) :-
	assert_called(Goal, From).

process_called_list([], _).
process_called_list([H|T], From) :-
	process_meta(H, From),
	process_called_list(T, From).

process_meta(A+N, From) :- !,
	callable(A),
	\+ A = _:_,			% avoid expanding call(M:P, ...)
	A =.. List,
	length(Rest, N),
	append(List, Rest, NList),
	Term =.. NList,
	process_body(Term, From).
process_meta(G, From) :-
	process_body(G, From).

term_member(T, A) :-
	compound(T),
	arg(_, T, A0),
	term_member(A0, A).
term_member(X, X).


		/********************************
		*       INCLUDED MODULES	*
		********************************/

process_use_module(_Module:_Files) :- !. % loaded in another module
process_use_module([]) :- !.
process_use_module([H|T]) :- !,
	process_use_module(H),
	process_use_module(T).
process_use_module(File) :-
	(   current_id(Src),
	    xref_public_list(File, Public, Src)
	->  assert_import(Public)
	;   true
	).

		/********************************
		*       PHASE 1 ASSERTIONS	*
		********************************/

no_record_called((_,_)).
no_record_called((_;_)).
no_record_called((_|_)).
no_record_called((_->_)).
no_record_called(\+(_)).
no_record_called(!).

assert_called(Var, _) :-
	var(Var), !.
assert_called(Goal, _) :-
	no_record_called(Goal), !.
assert_called(Goal, From) :-
	called(Goal, From), !.
assert_called(M:Goal, From) :-
	nonvar(M),
	x_module(M), !,
	assert_called(Goal, From).
assert_called(Goal, From) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	asserta(called(Term, From)).

assert_defined(_Module:_Head) :- !.	% defining in another module.  Bah!
assert_defined(Goal) :-
	defined(Goal), !.
assert_defined(Goal) :-
	functor(Goal, Name, Arity),
	functor(Term, Name, Arity),
	asserta(defined(Term)).

assert_import([]) :- !.
assert_import([H|T]) :-
	assert_import(H),
	assert_import(T).
assert_import(Name/Arity) :-
	functor(Term, Name, Arity),
	(   imported(Term)
	->  true
	;   asserta(imported(Term))
	).


assert_dynamic((A, B)) :- !,
	assert_dynamic(A),
	assert_dynamic(B).
assert_dynamic(Name/Arity) :-
	functor(Term, Name, Arity),
	assert_defined(Term),
	assert_entity(dynamic(Term)).


assert_require([]).
assert_require([Name/Arity|Rest]) :-
	functor(Head, Name, Arity),
	(   system_predicate(Head)
	->  true
	;   assert_import(Head)
	),
	assert_require(Rest).
				   

		/********************************
		*	  POST ANALYSYS		*
		********************************/

undefined(Head) :-
	defined(Head), !, fail.
undefined(Head) :-
	imported(Head), !, fail.
undefined(Head) :-
	global_predicate(Head), !, fail.
undefined(_:_) :- !,
	fail.
undefined(_).

post_analysis :-
	setof(To, unref_call(From, To), Unref),
	assert_entity(unreferenced_call(From, Unref)),
	fail.
post_analysis :-
	current_id(Key),
	unref_pred(Key, Head),
	assert_entity(unreferenced_predicate(Head)),
	fail.
post_analysis.

unref_call(From, To) :-
	called(To, From),
	undefined(To).

unref_pred(Key, Head) :-
	(   x_entity(Key, predicate(Head), _)
	;   x_entity(Key, xpce_class_local_predicate(Head), _)
	),
	local_definition(Head),
	\+ called(Head, _),
	\+ x_public(Key, Head).

local_definition(_:_) :- !,
	fail.
local_definition(_).





