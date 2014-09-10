/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module('$expand',
	  [ expand_term/2,		% +Term0, -Term
	    expand_goal/2,		% +Goal0, -Goal
	    expand_term/4,		% +Term0, ?Pos0, -Term, -Pos
	    expand_goal/4,	        % +Goal0, ?Pos0, -Goal, -Pos
            var_property/2		% +Var, ?Property
	  ]).

/** <module> Prolog source-code transformation

This module specifies, together with dcg.pl, the transformation of terms
as they are read from a file before they are processed by the compiler.

The toplevel is expand_term/2.  This uses three other translators:

	* Conditional compilation
	* term_expansion/2 rules provided by the user
	* DCG expansion

Note that this ordering implies  that conditional compilation directives
cannot be generated  by  term_expansion/2   rules:  they  must literally
appear in the source-code.

Term-expansion may choose to overrule DCG   expansion.  If the result of
term-expansion is a DCG rule, the rule  is subject to translation into a
predicate.

Next, the result is  passed  to   expand_bodies/2,  which  performs goal
expansion.
*/

:- dynamic
	system:term_expansion/2,
	system:goal_expansion/2,
	user:term_expansion/2,
	user:goal_expansion/2,
	system:term_expansion/4,
	system:goal_expansion/4,
	user:term_expansion/4,
	user:goal_expansion/4.
:- multifile
	system:term_expansion/2,
	system:goal_expansion/2,
	user:term_expansion/2,
	user:goal_expansion/2,
	system:term_expansion/4,
	system:goal_expansion/4,
	user:term_expansion/4,
	user:goal_expansion/4.

:- meta_predicate
	expand_terms(4, +, ?, -, -).

%%	expand_term(+Input, -Output) is det.
%%	expand_term(+Input, +Pos0, -Output, -Pos) is det.
%
%	This predicate is used to translate terms  as they are read from
%	a source-file before they are added to the Prolog database.

expand_term(Term0, Term) :-
	expand_term(Term0, _, Term, _).

expand_term(Var, Pos, Expanded, Pos) :-
	var(Var), !,
	Expanded = Var.
expand_term(Term, Pos0, [], Pos) :-
	cond_compilation(Term, X),
	X == [], !,
	atomic_pos(Pos0, Pos).
expand_term(Term, Pos0, Expanded, Pos) :-
	'$def_modules'([term_expansion/4,term_expansion/2], MList),
	call_term_expansion(MList, Term, Pos0, Term2, Pos1),
	expand_term_2(Term2, Pos1, Expanded, Pos).

call_term_expansion([], Term, Pos, Term, Pos).
call_term_expansion([M-Preds|T], Term0, Pos0, Term, Pos) :-
	current_prolog_flag(sandboxed_load, false), !,
	(   '$member'(Pred, Preds),
	    (	Pred == term_expansion/2
	    ->	M:term_expansion(Term0, Term1),
		Pos1 = Pos0
	    ;	M:term_expansion(Term0, Pos0, Term1, Pos1)
	    )
	->  expand_terms(call_term_expansion(T), Term1, Pos1, Term, Pos)
	;   call_term_expansion(T, Term0, Pos0, Term, Pos)
	).
call_term_expansion([M-Preds|T], Term0, Pos0, Term, Pos) :-
	(   '$member'(Pred, Preds),
	    (	Pred == term_expansion/2
	    ->	allowed_expansion(M:term_expansion(Term0, Term1)),
		call(M:term_expansion(Term0, Term1)),
		Pos1 = Pos
	    ;	allowed_expansion(M:term_expansion(Term0, Pos0, Term1, Pos1)),
		call(M:term_expansion(Term0, Pos0, Term1, Pos1))
	    )
	->  expand_terms(call_term_expansion(T), Term1, Pos1, Term, Pos)
	;   call_term_expansion(T, Term0, Pos0, Term, Pos)
	).

expand_term_2((Head --> Body), Pos0, Expanded, Pos) :-
	dcg_translate_rule((Head --> Body), Pos0, Expanded0, Pos1), !,
	expand_bodies(Expanded0, Pos1, Expanded, Pos).
expand_term_2(Term0, Pos0, Term, Pos) :-
	nonvar(Term0), !,
	expand_bodies(Term0, Pos0, Term, Pos).
expand_term_2(Term, Pos, Term, Pos).

%%	expand_bodies(+Term, +Pos0, -Out, -Pos) is det.
%
%	Find the body terms in Term and   give them to expand_goal/2 for
%	further processing. Note that  we   maintain  status information
%	about variables. Currently we only  detect whether variables are
%	_fresh_ or not. See var_info/3.

expand_bodies(Terms, Pos0, Out, Pos) :-
	'$def_modules'([goal_expansion/4,goal_expansion/2], MList),
	expand_terms(expand_body(MList), Terms, Pos0, Out, Pos),
	remove_attributes(Out, '$var_info').

expand_body(MList, (Head0 :- Body), Pos0, (Head :- ExpandedBody), Pos) :- !,
        term_variables(Head0, HVars),
        mark_vars_non_fresh(HVars),
	f2_pos(Pos0, HPos, BPos0, Pos, HPos, BPos),
	expand_goal(Body, BPos0, ExpandedBody0, BPos, MList, (Head0 :- Body)),
	(   compound(Head0),
	    '$set_source_module'(M, M),
	    replace_functions(Head0, Eval, Head, M),
	    Eval \== true
	->  ExpandedBody = (Eval,ExpandedBody0)
	;   Head = Head0,
	    ExpandedBody = ExpandedBody0
	).
expand_body(MList, (:- Body), Pos0, (:- ExpandedBody), Pos) :- !,
	f1_pos(Pos0, BPos0, Pos, BPos),
	expand_goal(Body, BPos0, ExpandedBody, BPos, MList, (:- Body)).

expand_body(_MList, Head0, Pos, Clause, Pos) :- % TBD: Position handling
	compound(Head0),
	'$set_source_module'(M, M),
	replace_functions(Head0, Eval, Head, M),
	Eval \== true, !,
	Clause = (Head :- Eval).
expand_body(_, Head, Pos, Head, Pos).


%%	expand_terms(:Closure, +In, +Pos0, -Out, -Pos)
%
%	Loop over two constructs that  can   be  added by term-expansion
%	rules in order to run the   next phase: calling term_expansion/2
%	can  return  a  list  and  terms    may   be  preceeded  with  a
%	source-location.

expand_terms(_, X, P, X, P) :-
	var(X), !.
expand_terms(C, List0, Pos0, List, Pos) :-
	nonvar(List0),
	List0 = [_|_], !,
	(   is_list(List0)
	->  list_pos(Pos0, Elems0, Pos, Elems),
	    expand_term_list(C, List0, Elems0, List, Elems)
	;   '$type_error'(list, List0)
	).
expand_terms(C, '$source_location'(File, Line):Clause0, Pos0, Clause, Pos) :- !,
	expand_terms(C, Clause0, Pos0, Clause1, Pos),
	add_source_location(Clause1, '$source_location'(File, Line), Clause).
expand_terms(C, Term0, Pos0, Term, Pos) :-
	call(C, Term0, Pos0, Term, Pos).

%%	add_source_location(+Term, +SrcLoc, -SrcTerm)
%
%	Re-apply source location after term expansion.  If the result is
%	a list, claim all terms to originate from this location.

add_source_location(Clauses0, SrcLoc, Clauses) :-
	(   is_list(Clauses0)
	->  add_source_location_list(Clauses0, SrcLoc, Clauses)
	;   Clauses = SrcLoc:Clauses0
	).

add_source_location_list([], _, []).
add_source_location_list([Clause|Clauses0], SrcLoc, [SrcLoc:Clause|Clauses]) :-
	add_source_location_list(Clauses0, SrcLoc, Clauses).

%%	expand_term_list(:Expander, +TermList, +Pos, -NewTermList, -PosList)

expand_term_list(_, [], _, [], []) :- !.
expand_term_list(C, [H0|T0], [PH0], Terms, PosL) :- !,
	expand_terms(C, H0, PH0, H, PH),
	add_term(H, PH, Terms, TT, PosL, PT),
	expand_term_list(C, T0, [PH0], TT, PT).
expand_term_list(C, [H0|T0], [PH0|PT0], Terms, PosL) :- !,
	expand_terms(C, H0, PH0, H, PH),
	add_term(H, PH, Terms, TT, PosL, PT),
	expand_term_list(C, T0, PT0, TT, PT).
expand_term_list(C, [H0|T0], PH0, Terms, PosL) :-
	expected_layout(list, PH0),
	expand_terms(C, H0, PH0, H, PH),
	add_term(H, PH, Terms, TT, PosL, PT),
	expand_term_list(C, T0, [PH0], TT, PT).

%%	add_term(+ExpandOut, ?ExpandPosOut, -Terms, ?TermsT, -PosL, ?PosLT)

add_term(List, Pos, Terms, TermT, PosL, PosT) :-
	nonvar(List), List = [_|_], !,
	(   is_list(List)
	->  append_tp(List, Terms, TermT, Pos, PosL, PosT)
	;   '$type_error'(list, List)
	).
add_term(Term, Pos, [Term|Terms], Terms, [Pos|PosT], PosT).

append_tp([], Terms, Terms, _, PosL, PosL).
append_tp([H|T0], [H|T1], Terms, [HP], [HP|TP1], PosL) :- !,
	append_tp(T0, T1, Terms, [HP], TP1, PosL).
append_tp([H|T0], [H|T1], Terms, [HP0|TP0], [HP0|TP1], PosL) :- !,
	append_tp(T0, T1, Terms, TP0, TP1, PosL).
append_tp([H|T0], [H|T1], Terms, Pos, [Pos|TP1], PosL) :-
	expected_layout(list, Pos),
	append_tp(T0, T1, Terms, [Pos], TP1, PosL).


list_pos(Var, _, _, _) :-
	var(Var), !.
list_pos(list_position(F,T,Elems0,none), Elems0,
	 list_position(F,T,Elems,none),  Elems).
list_pos(Pos, [Pos], Elems, Elems).


		 /*******************************
		 *      VAR_INFO/3 SUPPORT	*
		 *******************************/

%%	var_intersection(+List1, +List2, -Shared) is det.
%
%	Shared is the ordered intersection of List1 and List2.

var_intersection(List1, List2, Intersection) :-
	sort(List1, Set1),
	sort(List2, Set2),
	ord_intersection(Set1, Set2, Intersection).

%%	ord_intersection(+OSet1, +OSet2, -Int)
%
%	Ordered list intersection.  Copied from the library.

ord_intersection([], _Int, []).
ord_intersection([H1|T1], L2, Int) :-
	isect2(L2, H1, T1, Int).

isect2([], _H1, _T1, []).
isect2([H2|T2], H1, T1, Int) :-
	compare(Order, H1, H2),
	isect3(Order, H1, T1, H2, T2, Int).

isect3(<, _H1, T1,  H2, T2, Int) :-
	isect2(T1, H2, T2, Int).
isect3(=, H1, T1, _H2, T2, [H1|Int]) :-
	ord_intersection(T1, T2, Int).
isect3(>, H1, T1,  _H2, T2, Int) :-
	isect2(T2, H1, T1, Int).


%%	merge_variable_info(+Saved)
%
%	Merge info from two branches. The  info   in  Saved is the saved
%	info from the  first  branch,  while   the  info  in  the actual
%	variables is the  info  in  the   second  branch.  Only  if both
%	branches claim the variable to  be   fresh,  we  can consider it
%	fresh.

merge_variable_info([]).
merge_variable_info([Var=State|States]) :-
        (   get_attr(Var, '$var_info', CurrentState)
	->  true
	;   CurrentState = (-)
	),
        merge_states(Var, State, CurrentState),
        merge_variable_info(States).

merge_states(_Var, State, State) :- !.
merge_states(_Var, -, _) :- !.
merge_states(Var, State, -) :- !,
	put_attr(Var, '$var_info', State).
merge_states(Var, Left, Right) :-
	(   get_dict(fresh, Left, false)
	->  put_dict(fresh, Right, false)
	;   get_dict(fresh, Right, false)
	->  put_dict(fresh, Left, false)
	), !,
	(   Left >:< Right
	->  put_dict(Left, Right, State),
	    put_attr(Var, '$var_info', State)
	;   print_message(warning,
			  inconsistent_variable_properties(Left, Right)),
	    put_dict(Left, Right, State),
	    put_attr(Var, '$var_info', State)
	).


save_variable_info([], []).
save_variable_info([Var|Vars], [Var=State|States]):-
        (   get_attr(Var, '$var_info', State)
	->  true
        ;   State = (-)
        ),
        save_variable_info(Vars, States).

restore_variable_info([]).
restore_variable_info([Var=State|States]) :-
	(   State == (-)
	->  del_attr(Var, '$var_info')
	;   put_attr(Var, '$var_info', State)
	),
        restore_variable_info(States).

%%	var_property(+Var, ?Property)
%
%	True when Var has a property  Key with Value. Defined properties
%	are:
%
%	  - fresh(Fresh)
%	  Variable is first introduced in this goal and thus guaranteed
%	  to be unbound.  This property is always present.
%	  - name(-Name)
%	  True when Name is the name of the variable.

var_property(Var, Property) :-
	prop_var(Property, Var).

prop_var(fresh(Fresh), Var) :-
	 (   get_attr(Var, '$var_info', Info),
	     get_dict(fresh, Info, Fresh0)
	 ->  Fresh = Fresh0
	 ;   Fresh = true
	 ).
prop_var(name(Name), Var) :-
	(   b_getval('$variable_names', Bindings),
	    '$member'(Name0=Var0, Bindings),
	    Var0 == Var
	->  Name = Name0
	).


mark_vars_non_fresh([]) :- !.
mark_vars_non_fresh([Var|Vars]) :-
        (   get_attr(Var, '$var_info', Info)
	->  (   get_dict(fresh, Info, false)
	    ->	true
	    ;	put_dict(fresh, Info, false, Info1),
		put_attr(Var, '$var_info', Info1)
	    )
        ;   put_attr(Var, '$var_info', '$var_info'{fresh:false})
        ),
        mark_vars_non_fresh(Vars).


%%	remove_attributes(+Term, +Attribute) is det.
%
%	Remove all variable attributes Attribute from Term. This is used
%	to make term_expansion end with a  clean term. This is currently
%	_required_ for saving directives  in   QLF  files.  The compiler
%	ignores attributes, but I think  it   is  cleaner to remove them
%	anyway.

remove_attributes(Term, Attr) :-
	term_variables(Term, Vars),
	remove_var_attr(Vars, Attr).

remove_var_attr([], _):- !.
remove_var_attr([Var|Vars], Attr):-
        del_attr(Var, Attr),
        remove_var_attr(Vars, Attr).

%%	'$var_info':attr_unify_hook(_,_) is det.
%
%	Dummy unification hook for attributed variables.  Just succeeds.

'$var_info':attr_unify_hook(_, _).


		 /*******************************
		 *   GOAL_EXPANSION/2 SUPPORT	*
		 *******************************/

%%	expand_goal(+BodyTerm, +Pos0, -Out, -Pos) is det.
%%	expand_goal(+BodyTerm, -Out) is det.
%
%	Perform   macro-expansion   on    body     terms    by   calling
%	goal_expansion/2.

expand_goal(A, B) :-
	expand_goal(A, _, B, _).

expand_goal(A, P0, B, P) :-
	'$def_modules'([goal_expansion/4, goal_expansion/2], MList),
	(   expand_goal(A, P0, B, P, MList, _)
	->  remove_attributes(B, '$var_info'), A \== B
	), !.
expand_goal(A, P, A, P).

expand_goal(G0, P0, G, P, MList, Term) :-
	'$set_source_module'(M, M),
	expand_goal(G0, P0, G, P, M, MList, Term).

%%	expand_goal(+GoalIn, ?PosIn, -GoalOut, -PosOut,
%%		    +Module, -ModuleList, +Term) is det.
%
%	@param Module is the current module to consider
%	@param ModuleList are the other expansion modules
%	@param Term is the overall term that is being translated

% (*)	This is needed because call_goal_expansion may introduce extra
%	context variables.  Consider the code below, where the variable
%	E is introduced.  Is there a better representation for the
%	context?
%
%	  ==
%	  goal_expansion(catch_and_print(Goal), catch(Goal, E, print(E))).
%
%	  test :-
%		catch_and_print(true).
%	  ==

expand_goal(G, P, G, P, _, _, _) :-
        var(G), !.
expand_goal(G0, P0, G, P, M, MList, Term) :-
	call_goal_expansion(MList, G0, P0, G1, P1), !,
	expand_goal(G1, P1, G, P, M, MList, Term/G1).		% (*)
expand_goal((A,B), P0, Conj, P, M, MList, Term) :- !,
	f2_pos(P0, PA0, PB0, P1, PA, PB),
        expand_goal(A, PA0, EA, PA, M, MList, Term),
        expand_goal(B, PB0, EB, PB, M, MList, Term),
	simplify((EA,EB), P1, Conj, P).
expand_goal((A;B), P0, Or, P, M, MList, Term) :- !,
	f2_pos(P0, PA0, PB0, P1, PA, PB),
        term_variables(A, AVars),
        term_variables(B, BVars),
        var_intersection(AVars, BVars, SharedVars),
        save_variable_info(SharedVars, SavedState),
        expand_goal(A, PA0, EA, PA, M, MList, Term),
        save_variable_info(SharedVars, SavedState2),
        restore_variable_info(SavedState),
        expand_goal(B, PB0, EB, PB, M, MList, Term),
        merge_variable_info(SavedState2),
	simplify((EA;EB), P1, Or, P).
expand_goal((A->B), P0, Goal, P, M, MList, Term) :- !,
	f2_pos(P0, PA0, PB0, P1, PA, PB),
        expand_goal(A, PA0, EA, PA, M, MList, Term),
        expand_goal(B, PB0, EB, PB, M, MList, Term),
	simplify((EA->EB), P1, Goal, P).
expand_goal((A*->B), P0, Goal, P, M, MList, Term) :- !,
	f2_pos(P0, PA0, PB0, P1, PA, PB),
        expand_goal(A, PA0, EA, PA, M, MList, Term),
        expand_goal(B, PB0, EB, PB, M, MList, Term),
	simplify((EA*->EB), P1, Goal, P).
expand_goal((\+A), P0, Goal, P, M, MList, Term) :- !,
	f1_pos(P0, PA0, P1, PA),
	term_variables(A, AVars),
	save_variable_info(AVars, SavedState),
        expand_goal(A, PA0, EA, PA, M, MList, Term),
	restore_variable_info(SavedState),
	simplify(\+(EA), P1, Goal, P).
expand_goal(call(A), P0, call(EA), P, M, MList, Term) :- !,
	f1_pos(P0, PA0, P, PA),
        expand_goal(A, PA0, EA, PA, M, MList, Term).
expand_goal(M:G, P0, M:EG, P, _M, _MList, Term) :-
	atom(M), !,
	f2_pos(P0, PA, PB0, P, PA, PB),
	'$def_modules'(M:[goal_expansion/4,goal_expansion/2], MList),
	setup_call_cleanup(
	    '$set_source_module'(Old, M),
	    '$expand':expand_goal(G, PB0, EG, PB, M, MList, Term),
	    '$set_source_module'(_, Old)).
expand_goal(G0, P0, G, P, M, MList, Term) :-
	is_meta_call(G0, M, Head), !,
	expand_meta(Head, G0, P0, G, P, M, MList, Term).
expand_goal(G0, P0, G, P, M, MList, Term) :-
        term_variables(G0, Vars),
        mark_vars_non_fresh(Vars),
	expand_functions(G0, P0, G, P, M, MList, Term).

%%	is_meta_call(+G0, +M, +Head) is semidet.
%
%	True if M:G0 resolves to a real meta-goal as specified by Head.

is_meta_call(G0, M, Head) :-
	compound(G0),
	(   default_module(M, M2),
	    '$c_current_predicate'(_, M2:G0),
	    '$get_predicate_attribute'(M2:G0, meta_predicate, Head)
	->  true		% is a meta-pred without clauses defined?
	),
	has_meta_arg(Head).


%%	expand_meta(+MetaSpec, +G0, ?P0, -G, -P, +M, +Mlist, +Term)

expand_meta(Spec, G0, P0, G, P, M, MList, Term) :-
	functor(Spec, _, Arity),
	functor(G0, Name, Arity),
	functor(G1, Name, Arity),
	f_pos(P0, ArgPos0, P, ArgPos),
	expand_meta(1, Arity, Spec,
		    G0, ArgPos0, Eval,
		    G1,  ArgPos,
		    M, MList, Term),
	conj(Eval, G1, G).

expand_meta(I, Arity, Spec, G0, ArgPos0, Eval, G, [P|PT], M, MList, Term) :-
	I =< Arity, !,
	arg_pos(ArgPos0, P0, PT0),
	arg(I, Spec, Meta),
	arg(I, G0, A0),
	arg(I, G, A),
	expand_meta_arg(Meta, A0, P0, EvalA, A, P, M, MList, Term),
	I2 is I + 1,
	expand_meta(I2, Arity, Spec, G0, PT0, EvalB, G, PT, M, MList, Term),
	conj(EvalA, EvalB, Eval).
expand_meta(_, _, _, _, _, true, _, [], _, _, _).

arg_pos(List, _, _) :- var(List), !.	% no position info
arg_pos([H|T], H, T) :- !.		% argument list
arg_pos([], _, []).			% new has more

expand_meta_arg(0, A0, PA0, true, A, PA, M, MList, Term) :- !,
	expand_goal(A0, PA0, A1, PA, M, MList, Term),
	compile_meta_call(A1, A, M, Term).
expand_meta_arg(^, A0, PA0, true, A, PA, M, MList, Term) :- !,
	expand_setof_goal(A0, PA0, A, PA, M, MList, Term).
expand_meta_arg(S, A0, _PA0, Eval, A, _PA, M, _MList, _Term) :-
	replace_functions(A0, Eval, A, M), % TBD: pass positions
	(   Eval == true
	->  true
	;   meta_arg(S)
	->  throw(error(context_error(function, meta_arg(S)), _))
	;   true
	).

has_meta_arg(Head) :-
	arg(_, Head, Arg),
	direct_call_meta_arg(Arg), !.

direct_call_meta_arg(0).
direct_call_meta_arg(^).

meta_arg(:).
meta_arg(//).
meta_arg(I) :- integer(I).


expand_setof_goal(Var, Pos, Var, Pos, _, _, _) :-
	var(Var), !.
expand_setof_goal(V^G, P0, V^EG, P, M, MList, Term) :- !,
	f2_pos(P0, PA0, PB, P, PA, PB),
        expand_setof_goal(G, PA0, EG, PA, M, MList, Term).
expand_setof_goal(M0:G, P0, M0:EG, P, M, MList, Term) :- !,
	f2_pos(P0, PA0, PB, P, PA, PB),
        expand_setof_goal(G, PA0, EG, PA, M, MList, Term).
expand_setof_goal(G, P0, EG, P, M, MList, Term) :- !,
        expand_goal(G, P0, EG0, P, M, MList, Term),
	compile_meta_call(EG0, EG, M, Term).		% TBD: Pos?


%%	call_goal_expansion(+ExpandModules,
%%			    +Goal0, ?Pos0, -Goal, -Pos) is semidet.
%
%	Succeeds  if  the   context   has    a   module   that   defines
%	goal_expansion/2 this rule succeeds and  Goal   is  not equal to
%	Goal0. Note that the translator is   called  recursively until a
%	fixed-point is reached.

call_goal_expansion(MList, G0, P0, G, P) :-
	current_prolog_flag(sandboxed_load, false), !,
	(   '$member'(M-Preds, MList),
	    '$member'(Pred, Preds),
	    (	Pred == goal_expansion/4
	    ->	M:goal_expansion(G0, P0, G, P)
	    ;	M:goal_expansion(G0, G),
		P = P0
	    ),
	    G0 \== G
	->  true
	).
call_goal_expansion(MList, G0, P0, G, P) :-
	(   '$member'(M-Preds, MList),
	    '$member'(Pred, Preds),
	    (   Pred == goal_expansion/4
	    ->  Expand = M:goal_expansion(G0, P0, G, P)
	    ;	Expand = M:goal_expansion(G0, G)
	    ),
	    allowed_expansion(Expand),
	    call(Expand),
	    G0 \== G
	->  true
	).

%%	allowed_expansion(:Goal) is semidet.
%
%	Calls prolog:sandbox_allowed_expansion(:Goal) prior   to calling
%	Goal for the purpose of term or   goal  expansion. This hook can
%	prevent the expansion to take place by raising an exception.
%
%	@throws	exceptions from prolog:sandbox_allowed_expansion/1.

:- multifile
	prolog:sandbox_allowed_expansion/1.

allowed_expansion(QGoal) :-
	strip_module(QGoal, M, Goal),
	catch(prolog:sandbox_allowed_expansion(M:Goal), E, true),
	(   var(E)
	->  fail
	;   !,
	    print_message(error, E),
	    fail
	).
allowed_expansion(_).


		 /*******************************
		 *	FUNCTIONAL NOTATION	*
		 *******************************/

%%	expand_functions(+G0, +P0, -G, -P, +M, +MList, +Term) is det.
%
%	@tbd: position logic
%	@tbd: make functions module-local

expand_functions(G0, P0, G, P, M, _MList, _Term) :-
	contains_functions(G0),
	replace_functions(G0, P0, Eval, EvalPos, G1, G1Pos, M),
	Eval \== true, !,
	wrap_var(G1, G1Pos, G2, G2Pos),
	conj(Eval, EvalPos, G2, G2Pos, G, P).
expand_functions(G, P, G, P, _, _, _).

wrap_var(G, P, G, P) :-
	nonvar(G), !.
wrap_var(G, P0, call(G), P) :-
	(   nonvar(P0)
	->  P = term_position(F,T,F,T,[P0]),
	    atomic_pos(P0, F-T)
	;   true
	).

%%	contains_functions(+Term) is semidet.
%
%	True when Term contains a function reference.

contains_functions(Term) :-
	compound(Term),
	(   function(Term, _)
	->  true
	;   arg(_, Term, Arg),
	    contains_functions(Arg)
	->  true
	).

%%	replace_functions(+GoalIn, +PosIn,
%%			  -Eval, -EvalPos,
%%			  -GoalOut, -PosOut,
%%			  +ContextTerm) is det.
%
%	@tbd	Proper propagation of list, dict and brace term positions.

:- public
	replace_functions/4.		% used in dicts.pl

replace_functions(GoalIn, Eval, GoalOut, Context) :-
	replace_functions(GoalIn, _, Eval, _, GoalOut, _, Context).

replace_functions(Var, Pos, true, _, Var, Pos, _Ctx) :-
	var(Var), !.
replace_functions(F, FPos, Eval, EvalPos, Var, VarPos, Ctx) :-
	function(F, Ctx), !,
	compound_name_arity(F, Name, Arity),
	PredArity is Arity+1,
	compound_name_arity(G, Name, PredArity),
	arg(PredArity, G, Var),
	extend_1_pos(FPos, FArgPos, GPos, GArgPos, VarPos),
	map_functions(0, Arity, F, FArgPos, G, GArgPos, Eval0, EP0, Ctx),
	conj(Eval0, EP0, G, GPos, Eval, EvalPos).
replace_functions(Term0, Term0Pos, Eval, EvalPos, Term, TermPos, Ctx) :-
	compound(Term0), !,
	compound_name_arity(Term0, Name, Arity),
	compound_name_arity(Term, Name, Arity),
	f_pos(Term0Pos, Args0Pos, TermPos, ArgsPos),
	map_functions(0, Arity,
		      Term0, Args0Pos, Term, ArgsPos, Eval, EvalPos, Ctx).
replace_functions(Term, Pos, true, _, Term, Pos, _).


%%	map_functions(+Arg, +Arity,
%%		      +TermIn, +ArgInPos, -Term, -ArgPos, -Eval, -EvalPos,
%%		      +Context)

map_functions(Arity, Arity, _, LPos0, _, LPos, true, _, _) :- !,
	pos_nil(LPos0, LPos).
map_functions(I0, Arity, Term0, LPos0, Term, LPos, Eval, EP, Ctx) :-
	pos_list(LPos0, AP0, APT0, LPos, AP, APT),
	I is I0+1,
	arg(I, Term0, Arg0),
	arg(I, Term, Arg),
	replace_functions(Arg0, AP0, Eval0, EP0, Arg, AP, Ctx),
	map_functions(I, Arity, Term0, APT0, Term, APT, Eval1, EP1, Ctx),
	conj(Eval0, EP0, Eval1, EP1, Eval, EP).

conj(true, X, X) :- !.
conj(X, true, X) :- !.
conj(X, Y, (X,Y)).

conj(true, _, X, P, X, P) :- !.
conj(X, P, true, _, X, P) :- !.
conj(X, PX, Y, PY, (X,Y), _) :-
	var(PX), var(PY), !.
conj(X, PX, Y, PY, (X,Y), P) :-
	P = term_position(F,T,FF,FT,[PX,PY]),
	atomic_pos(PX, F-FF),
	atomic_pos(PY, FT-T).

%%	function(?Term, +Context)
%
%	True if function expansion needs to be applied for the given
%	term.

function(.(_,_), _) :- \+ functor([_|_], ., _).


		 /*******************************
		 *	  POSITION LOGIC	*
		 *******************************/

%%	f2_pos(?TermPos0, ?PosArg10, ?PosArg20,
%%	       ?TermPos,  ?PosArg1,  ?PosArg2) is det.
%%	f1_pos(?TermPos0, ?PosArg10, ?TermPos,  ?PosArg1) is det.
%%	f_pos(?TermPos0, ?PosArgs0, ?TermPos,  ?PosArgs) is det.
%%	atomic_pos(?TermPos0, -AtomicPos) is det.
%
%	Position progapation routines.

f2_pos(Var, _, _, _, _, _) :-
	var(Var), !.
f2_pos(term_position(F,T,FF,FT,[A10,A20]), A10, A20,
       term_position(F,T,FF,FT,[A1, A2 ]), A1,  A2) :- !.
f2_pos(Pos, _, _, _, _, _) :-
	expected_layout(f2, Pos).

f1_pos(Var, _, _, _) :-
	var(Var), !.
f1_pos(term_position(F,T,FF,FT,[A10]), A10,
       term_position(F,T,FF,FT,[A1 ]),  A1) :- !.
f1_pos(Pos, _, _, _) :-
	expected_layout(f1, Pos).

f_pos(Var, _, _, _) :-
	var(Var), !.
f_pos(term_position(F,T,FF,FT,ArgPos0), ArgPos0,
      term_position(F,T,FF,FT,ArgPos),  ArgPos) :- !.
f_pos(Pos, _, _, _) :-
	expected_layout(compound, Pos).

atomic_pos(Pos, _) :-
	var(Pos), !.
atomic_pos(Pos, F-T) :-
	arg(1, Pos, F),
	arg(2, Pos, T).

%%	pos_nil(+Nil, -Nil) is det.
%%	pos_list(+List0, -H0, -T0, -List, -H, -T) is det.
%
%	Position propagation for lists.

pos_nil(Var, _) :- var(Var), !.
pos_nil([], []) :- !.
pos_nil(Pos, _) :-
	expected_layout(nil, Pos).

pos_list(Var, _, _, _, _, _) :- var(Var), !.
pos_list([H0|T0], H0, T0, [H|T], H, T) :- !.
pos_list(Pos, _, _, _, _, _) :-
	expected_layout(list, Pos).

%%	extend_1_pos(+FunctionPos, -FArgPos, -EvalPos, -EArgPos, -VarPos)
%
%	Deal with extending a function to include the return value.

extend_1_pos(Pos, _, _, _, _) :-
	var(Pos), !.
extend_1_pos(term_position(F,T,FF,FT,FArgPos), FArgPos,
	     term_position(F,T,FF,FT,GArgPos), GArgPos0,
	     FT-FT1) :-
	integer(FT), !,
	FT1 is FT+1,
	'$same_length'(FArgPos, GArgPos0),
	'$append'(GArgPos0, [FT-FT1], GArgPos).
extend_1_pos(F-T, [],
	     term_position(F,T,F,T,[T-T1]), [],
	     T-T1) :-
	integer(T), !,
	T1 is T+1.
extend_1_pos(Pos, _, _, _, _) :-
	expected_layout(callable, Pos).

'$same_length'(List, List) :-
	var(List), !.
'$same_length'([], []).
'$same_length'([_|T0], [_|T]) :-
	'$same_length'(T0, T).


%%	expected_layout(+Expected, +Found)
%
%	Print a message  if  the  layout   term  does  not  satisfy  our
%	expectations.  This  means  that   the  transformation  requires
%	support from term_expansion/4 and/or goal_expansion/4 to achieve
%	proper source location information.

:- create_prolog_flag(debug_term_position, false, []).

expected_layout(Expected, Pos) :-
	current_prolog_flag(debug_term_position, true), !,
	'$print_message'(warning, expected_layout(Expected, Pos)).
expected_layout(_, _).


		 /*******************************
		 *    SIMPLIFICATION ROUTINES	*
		 *******************************/

%%	simplify(+ControlIn, +Pos0, -ControlOut, -Pos) is det.
%
%	Simplify control structures
%
%	@tbd	Much more analysis
%	@tbd	Turn this into a separate module

simplify(Control, P, Control, P) :-
	current_prolog_flag(optimise, false), !.
simplify(Control, P0, Simple, P) :-
	simple(Control, P0, Simple, P), !.
simplify(Control, P, Control, P).

simple((X,Y), P0, Conj, P) :-
	(   true(X)
	->  Conj = Y,
	    f2_pos(P0, _, P, _, _, _)
	;   false(X)
	->  Conj = fail,
	    f2_pos(P0, P1, _, _, _, _),
	    atomic_pos(P1, P)
	;   true(Y)
	->  Conj = X,
	    f2_pos(P0, P, _, _, _, _)
	).
simple((I->T;E), P0, ITE, P) :-
	(   true(I)
	->  ITE = T,
	    f2_pos(P0, P1, _, _, _, _),
	    f2_pos(P1, _, P, _, _, _)
	;   false(I)
	->  ITE = E,
	    f2_pos(P0, _, P, _, _, _)
	).
simple((X;Y), P0, Or, P) :-
	false(X),
	Or = Y,
	f2_pos(P0, _, P, _, _, _).

true(X) :-
	nonvar(X),
	eval_true(X).

false(X) :-
	nonvar(X),
	eval_false(X).

%%	eval_true(+Goal) is semidet.
%%	eval_false(+Goal) is semidet.

eval_true(true).
eval_true(otherwise).

eval_false(fail).
eval_false(false).


		 /*******************************
		 *	   META CALLING		*
		 *******************************/

:- create_prolog_flag(compile_meta_arguments, false, [type(atom)]).

%%	compile_meta_call(+CallIn, -CallOut, +Module, +Term) is det.
%
%	Compile (complex) meta-calls into a clause.

compile_meta_call(CallIn, CallIn, _, Term) :-
	var(Term), !.			% explicit call; no context
compile_meta_call(CallIn, CallIn, _, _) :-
	var(CallIn), !.
compile_meta_call(CallIn, CallIn, _, _) :-
	(   current_prolog_flag(compile_meta_arguments, false)
	;   current_prolog_flag(xref, true)
	), !.
compile_meta_call(CallIn, CallIn, _, _) :-
	strip_module(CallIn, _, Call),
	(   is_aux_meta(Call)
	;   \+ control(Call),
	    (	'$c_current_predicate'(_, system:Call),
		\+ current_prolog_flag(compile_meta_arguments, always)
	    ;   current_prolog_flag(compile_meta_arguments, control)
	    )
	), !.
compile_meta_call(M:CallIn, CallOut, _, Term) :- !,
	(   atom(M), callable(CallIn)
	->  compile_meta_call(CallIn, CallOut, M, Term)
	;   CallOut = M:CallIn
	).
compile_meta_call(CallIn, CallOut, Module, Term) :-
	compile_meta(CallIn, CallOut, Module, Term, Clause),
	Clause = (Head:-Body),
	functor(Head, Name, Arity),
	'$set_source_module'(SM, SM),
	(   current_predicate(SM:Name/Arity)
	->  true
	;   SM == Module
	->  compile_aux_clauses([Clause])
	;   compile_aux_clauses([Head:-Module:Body])
	).

control((_,_)).
control((_;_)).
control((_->_)).
control((_*->_)).
control(\+(_)).

is_aux_meta(Term) :-
	callable(Term),
	functor(Term, Name, _),
	sub_atom(Name, 0, _, _, '__aux_meta_call_').

compile_meta(CallIn, CallOut, M, Term, (CallOut :- Body)) :-
	term_variables(Term, AllVars),
	term_variables(CallIn, InVars),
	intersection_eq(InVars, AllVars, HeadVars),
	variant_sha1(CallIn+HeadVars, Hash),
	atom_concat('__aux_meta_call_', Hash, AuxName),
	expand_goal(CallIn, _Pos0, Body, _Pos, M, [], (CallOut:-CallIn)),
	length(HeadVars, Arity),
	(   Arity > 256			% avoid 1024 arity limit
	->  HeadArgs = [v(HeadVars)]
	;   HeadArgs = HeadVars
	),
	CallOut =.. [AuxName|HeadArgs].

%%	intersection_eq(+Small, +Big, -Shared) is det.
%
%	Shared are the variables in Small that   also appear in Big. The
%	variables in Shared are in the same order as Small.

intersection_eq([], _, []).
intersection_eq([H|T0], L, List) :-
	(   member_eq(H, L)
	->  List = [H|T],
	    intersection_eq(T0, L, T)
	;   intersection_eq(T0, L, List)
	).

member_eq(E, [H|T]) :-
	(   E == H
	->  true
	;   member_eq(E, T)
	).


		 /*******************************
		 *	:- IF ... :- ENDIF	*
		 *******************************/

:- thread_local
	'$include_code'/3.

'$including' :-
	'$include_code'(X, _, _), !,
	X == true.
'$including'.

cond_compilation((:- if(G)), []) :-
	source_location(File, Line),
	(   '$including'
	->  (   catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true, File, Line))
	    ;   asserta('$include_code'(false, File, Line))
	    )
	;   asserta('$include_code'(else_false, File, Line))
	).
cond_compilation((:- elif(G)), []) :-
	source_location(File, Line),
	(   clause('$include_code'(Old, OF, _), _, Ref)
	->  same_source(File, OF, elif),
	    erase(Ref),
	    (   Old == true
	    ->  asserta('$include_code'(else_false, File, Line))
	    ;   Old == false,
		catch('$eval_if'(G), E, (print_message(error, E), fail))
	    ->  asserta('$include_code'(true, File, Line))
	    ;	asserta('$include_code'(Old, File, Line))
	    )
	;   throw(error(conditional_compilation_error(no_if, elif), _))
	).
cond_compilation((:- else), []) :-
	source_location(File, Line),
	(   clause('$include_code'(X, OF, _), _, Ref)
	->  same_source(File, OF, else),
	    erase(Ref),
	    (   X == true
	    ->  X2 = false
	    ;   X == false
	    ->	X2 = true
	    ;	X2 = X
	    ),
	    asserta('$include_code'(X2, File, Line))
	;   throw(error(conditional_compilation_error(no_if, else), _))
	).
cond_compilation(end_of_file, end_of_file) :- !, % TBD: Check completeness
	source_location(File, _),
	(   clause('$include_code'(_, OF, OL), _)
	->  (   File == OF
	    ->	throw(error(conditional_compilation_error(
				unterminated,OF:OL), _))
	    ;   true
	    )
	;   true
	).
cond_compilation((:- endif), []) :- !,
	source_location(File, _),
	(   (   clause('$include_code'(_, OF, _), _, Ref)
	    ->  same_source(File, OF, endif),
		erase(Ref)
	    )
	->  true
	;   throw(error(conditional_compilation_error(no_if, endif), _))
	).
cond_compilation(_, []) :-
	\+ '$including'.

same_source(File, File, _) :- !.
same_source(_,    _,    Op) :-
	throw(error(conditional_compilation_error(no_if, Op), _)).


'$eval_if'(G) :-
	expand_goal(G, G2),
	'$set_source_module'(Module, Module),
	Module:G2.
