/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module('$attvar',
	  [ '$wakeup'/1,		% +Wakeup list
	    freeze/2,			% +Var, :Goal
	    frozen/2,			% @Var, -Goal
	    call_residue_vars/2,        % :Goal, -Vars
	    copy_term/3                 % +Term, -Copy, -Residue
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Attributed  variable  and  coroutining  support    based  on  attributed
variables. This module is complemented with C-defined predicates defined
in pl-attvar.c
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	'$wakeup'(+List)
%	
%	Called from the kernel if assignments have been made to
%	attributed variables.

'$wakeup'([]).
'$wakeup'(wakeup(Attribute, Value, Rest)) :-
	call_all_attr_uhooks(Attribute, Value),
	'$wakeup'(Rest).

call_all_attr_uhooks([], _).
call_all_attr_uhooks(att(Module, AttVal, Rest), Value) :-
	uhook(Module, AttVal, Value),
	call_all_attr_uhooks(Rest, Value).


%	uhook(+AttributeName, +AttributeValue, +Value)
%	
%	Run the unify hook for attributed named AttributeName after
%	assigning an attvar with attribute AttributeValue the value
%	Value.
%	
%	This predicate deals with reserved attribute names to avoid
%	the meta-call overhead.

uhook(freeze, Goal, Y) :- !,
	(   attvar(Y)
	->  (   get_attr(Y, freeze, G2)
	    ->	put_attr(Y, freeze, '$and'(G2, Goal))
	    ;	put_attr(Y, freeze, Goal)
	    )
	;   unfreeze(Goal)
	).
uhook(Module, AttVal, Value) :-
	Module:attr_unify_hook(AttVal, Value).


%	unfreeze(+ConjunctionOrGoal)
%	
%	Handle  unfreezing  of  conjunctions.  As  meta-calling  control
%	structures is slower than meta-interpreting them   we do this in
%	Prolog. Another advantage is that   having unfreeze/1 in between
%	makes the stacktrace and profiling   easier  to intepret. Please
%	note that we cannot use a direct conjunction as this would break
%	freeze(X, (a, !, b)).

unfreeze('$and'(A,B)) :- !,
	unfreeze(A),
	unfreeze(B).
unfreeze(Goal) :-
	Goal.

%	freeze(@Var, :Goal)
%	
%	Suspend execution of Goal until Var is unbound.

:- module_transparent freeze/2.

freeze(Var, Goal) :-
	'$freeze'(Var, Goal), !.	% Succeeds if delayed
freeze(_, Goal) :-
	Goal.

%	frozen(@Var, -Goals)
%	
%	Unify Goals with the goals frozen on Var or true if no
%	goals are grozen on Var.

frozen(Var, Goals) :-
	get_attr(Var, freeze, Goals0), !,
	make_conjunction(Goals0, Goals).
frozen(_, true).

make_conjunction('$and'(A, B0), (A, B)) :- !,
	make_conjunction(B0, B).
make_conjunction(G, G).


		 /*******************************
		 *	       PORTRAY		*
		 *******************************/

%	portray_attvar(@Var)
%	
%	Called from write_term/3 using the option attributes(portray) or
%	when the prolog flag write_attributes   equals portray. Its task
%	is the write the attributes in a human readable format.

portray_attvar(Var) :-
	write('{'),
	get_attrs(Var, Attr),
	portray_attrs(Attr, Var),
	write('}').

portray_attrs([], _).
portray_attrs(att(Name, Value, Rest), Var) :-
	portray_attr(Name, Value, Var),
	(   Rest == []
	->  true
	;   write(', '),
	    portray_attrs(Rest, Var)
	).

portray_attr(freeze, Goal, Var) :- !,
	format('freeze(~w, ~W)', [ Var, Goal,
				   [ portray(true),
				     quoted(true),
				     attributes(ignore)
				   ]
				 ]).
portray_attr(Name, Value, Var) :-
	G = Name:attr_portray_hook(Value, Var),
	(   '$c_current_predicate'(_, G),
	    G
	->  true
	;   format('~w = ...', [Name])
	).


		 /*******************************
		 *	    CALL RESIDUE	*
		 *******************************/

%%	call_residue_vars(:Goal, -Vars)
%
%	If Goal is  true,  Vars  is   the  set  of  residual  attributed
%	variables created by Goal. Goal  is   called  as in call/1. This
%	predicate  is  for  debugging  constraint   programs.  Assume  a
%	constraint program that creates  conflicting   constraints  on a
%	variable that is not part of the   result  variables of Goal. If
%	the solver is powerful enough it   will  detect the conflict and
%	fail. If the solver is too  weak   however  it  will succeed and
%	residual attributed variables holding the conflicting constraint
%	form a witness of this problem.
%	
%	@bug	In the current implementation attributed variables may
%		be garbage collected and will not appear in Vars.

:- module_transparent
	call_residue_vars/2,
	call_det/2.

call_residue_vars(Goal, Vars) :-
	'$get_choice_point'(Chp),
	call_det(Goal, Det),
        '$attvars_after_choicepoint'(Chp, Vars),
	(   Det == true
	->  !
	;   true
	).
call_residue_vars(_,_) :-
	fail.

call_det(Goal, Det) :-
	Goal,
	deterministic(Det).

%%    copy_term(+Term, -Copy, -Gs) is det.
%
%    Creates a regular term Copy as a copy of Term (without any
%    attributes), and a list Gs of goals that when executed reinstate
%    all attributes onto Copy. The nonterminal attribute_goals//1, as
%    defined in the modules the attributes stem from, is used to
%    convert attributes to lists of goals.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Intention: Reflect term variable order in residual goals. First, all
   attributes are stripped from Term, then they are processed in their
   natural order. The same strategy is applied to attributes themselves.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

copy_term(Term, Copy, Gs) :-
	% encapsulated in findall/3 such that attributes can be removed etc.
	findall(Term-GsC, phrase(term_residuals(Term),GsC), [Copy-Gs]).

term_residuals(Term) -->
	(   { '$attributed'(Term) }
	->  { term_variables(Term, Vs0), collect_att_terms(Vs0, As, Vs) },
	    att_terms_residuals(As, Vs)
	;   []
	).

collect_att_terms([], [], []).
collect_att_terms([V0|Vs0], As, Vs) :-
	(   attvar(V0)
	->  get_attrs(V0, A),
	    As = [A|ARest],
	    Vs = [V0|VRest],
	    del_all_attrs(A, V0)
	;   As = ARest,
	    Vs = VRest
	),
	collect_att_terms(Vs0, ARest, VRest).

del_all_attrs([], _).
del_all_attrs(att(Module,_,As), V) :-
	del_attr(V, Module),
	del_all_attrs(As, V).

att_terms_residuals([], _)	    --> [].
att_terms_residuals([A|As], [V|Vs]) -->
	att_term_residuals(A, V),
	att_terms_residuals(As, Vs).

att_term_residuals([], _)		    --> [].
att_term_residuals(att(Module,Value,As), V) -->
	% temporarily reinstate this attribute for attribute_goals//1
	{ put_attr(V, Module, Value) },
	(   { Module == freeze }
	->  [freeze(V, Value)]
	;   { current_predicate(Module:attribute_goals/3) }
	->  { phrase(Module:attribute_goals(V), Goals) },
	    dlist(Goals)
	;   { current_predicate(Module:attribute_goal/2) }
	->  { Module:attribute_goal(V, Goal) },
	    dot_list(Goal)
	;   [put_attr(V, Module, Value)]
	),
	{ del_attr(V, Module) },
	term_residuals(Value),
	att_term_residuals(As, V).

dlist([])     --> [].
dlist([L|Ls]) --> [L], dlist(Ls).

dot_list((A,B)) --> !, dot_list(A), dot_list(B).
dot_list(A)	--> [A].
