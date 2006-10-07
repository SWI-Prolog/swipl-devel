/*  $Id$

    Part of CLP(Q,R) (Constraint Logic Programming over Rationals and Reals)
    
    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

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


:- module(dump,
	[
	    dump/3,
	    projecting_assert/1
	]).
:- use_module(class,
	[
	    class_allvars/2
	]).
:- use_module(geler,
	[
	    collect_nonlin/3
	]).
:- use_module(library(assoc),
	[
	    empty_assoc/1,
	    get_assoc/3,
	    put_assoc/4,
	    assoc_to_list/2
	]).
:- use_module(itf,
	[
	    dump_linear/3,
	    dump_nonzero/3
	]).
:- use_module(project,
	[
	    project_attributes/2
	]).
:- use_module(ordering,
	[
	    ordering/1
	]).

% dump(Target,NewVars,Constraints)
%
% Returns in <Constraints>, the constraints that currently hold on Target where
% all variables in <Target> are copied to new variables in <NewVars> and the
% constraints are given on these new variables. In short, you can safely
% manipulate <NewVars> and <Constraints> without changing the constraints on
% <Target>.

dump([],[],[]) :- !.
dump(Target,NewVars,Constraints) :-
	(   (	proper_varlist(Target)
	    ->  true
	    ;   % Target is not a list of variables
		throw(instantiation_error(dump(Target,NewVars,Constraints),1))
	    ),
	    ordering(Target),
	    related_linear_vars(Target,All),	% All contains all variables of the classes of Target variables.
	    nonlin_crux(All,Nonlin),
	    project_attributes(Target,All),
	    related_linear_vars(Target,Again),	% project drops/adds vars
	    all_attribute_goals(Again,Gs,Nonlin),
	    empty_assoc(D0),
	    mapping(Target,NewVars,D0,D1),	% late (AVL suffers from put_atts)
	    copy(Gs,Copy,D1,_),			% strip constraints
	    nb_setval(clpqr_dump,NewVars/Copy),
	    fail				% undo projection
	;   catch(nb_getval(clpqr_dump,NewVars/Constraints),_,fail),
	    nb_delete(clpqr_dump)
	).

:- meta_predicate projecting_assert(:).

projecting_assert(QClause) :-
	strip_module(QClause, Module, Clause),  % JW: SWI-Prolog not always qualifies the term!
	copy_term(Clause,Copy,Constraints),
	l2c(Constraints,Conj),			% fails for []
	(   Sm = clpq
	;   Sm = clpr
	),			% proper module for {}/1
	!,
	(   Copy = (H:-B)
	->  % former rule
	    Module:assert((H:-Sm:{Conj},B))
	;   % former fact
	    Module:assert((Copy:-Sm:{Conj}))
	).
projecting_assert(Clause) :-	% not our business
	assert(Clause).

copy_term(Term,Copy,Constraints) :-
	(   term_variables(Term,Target),	% get all variables in Term
	    related_linear_vars(Target,All),	% get all variables of the classes of the variables in Term
	    nonlin_crux(All,Nonlin),		% get a list of all the nonlinear goals of these variables
	    project_attributes(Target,All),
	    related_linear_vars(Target,Again),	% project drops/adds vars
	    all_attribute_goals(Again,Gs,Nonlin),
	    empty_assoc(D0),
	    copy(Term/Gs,TmpCopy,D0,_),	  % strip constraints
	    nb_setval(clpqr_dump,TmpCopy),
	    fail
	;   catch(nb_getval(clpqr_dump,Copy/Constraints),_,fail),
	    nb_delete(clpqr_copy_term)
	).

% l2c(Lst,Conj)
%
% converts a list to a round list: [a,b,c] -> (a,b,c) and [a] becomes a

l2c([X|Xs],Conj) :-
	(   Xs = []
	->  Conj = X
	;   Conj = (X,Xc),
	    l2c(Xs,Xc)
	).

% proper_varlist(List)
%
% Returns whether Lst is a list of variables.
% First clause is to avoid unification of a variable with a list.

proper_varlist(X) :-
	var(X),
	!,
	fail.
proper_varlist([]).
proper_varlist([X|Xs]) :-
	var(X),
	proper_varlist(Xs).

% related_linear_vars(Vs,All)
%
% Generates a list of all variables that are in the classes of the variables in
% Vs.

related_linear_vars(Vs,All) :-
	empty_assoc(S0),
	related_linear_sys(Vs,S0,Sys),
	related_linear_vars(Sys,All,[]).

% related_linear_sys(Vars,Assoc,List)
%
% Generates in List, a list of all to classes to which variables in Vars
% belong.
% Assoc should be an empty association list and is used internally.
% List contains elements of the form C-C where C is a class and both C's are
% equal.

related_linear_sys([],S0,L0) :- assoc_to_list(S0,L0).
related_linear_sys([V|Vs],S0,S2) :-
	(   get_attr(V,itf,Att),
	    arg(6,Att,class(C))
	->  put_assoc(C,S0,C,S1)
	;   S1 = S0
	),
	related_linear_sys(Vs,S1,S2).

% related_linear_vars(Classes,[Vars|VarsTail],VarsTail)
%
% Generates a difference list of all variables in the classes in Classes.
% Classes contains elements of the form C-C where C is a class and both C's are
% equal.

related_linear_vars([]) --> [].
related_linear_vars([S-_|Ss]) -->
	{
	    class_allvars(S,Otl)
	},
	cpvars(Otl),
	related_linear_vars(Ss).

% cpvars(Vars,Out,OutTail)
%
% Makes a new difference list of the difference list Vars.
% All nonvars are removed.

cpvars(Xs) --> {var(Xs)}, !.
cpvars([X|Xs]) -->
	(   { var(X) }
	->  [X]
	;   []
	),
	cpvars(Xs).

% nonlin_crux(All,Gss)
%
% Collects all pending non-linear constraints of variables in All.
% This marks all nonlinear goals of the variables as run and cannot
% be reversed manually.

nonlin_crux(All,Gss) :-
	collect_nonlin(All,Gs,[]),	% collect the nonlinear goals of variables All
					% this marks the goals as run and cannot be reversed manually
	nonlin_strip(Gs,Gss).

% nonlin_strip(Gs,Solver,Res)
%
% Removes the goals from Gs that are not from solver Solver.

nonlin_strip([],[]).
nonlin_strip([_:What|Gs],Res) :-
	(   What = {G}
	->  Res = [G|Gss]
	;   Res = [What|Gss]
	),
	nonlin_strip(Gs,Gss).

all_attribute_goals([]) --> [].
all_attribute_goals([V|Vs]) -->
	dump_linear(V),
	dump_nonzero(V),
	all_attribute_goals(Vs).

% mapping(L1,L2,AssocIn,AssocOut)
%
% Makes an association mapping of lists L1 and L2:
% L1 = [L1H|L1T] and L2 = [L2H|L2T] then the association L1H-L2H is formed
% and the tails are mapped similarly.

mapping([],[],D0,D0).
mapping([T|Ts],[N|Ns],D0,D2) :-
	put_assoc(T,D0,N,D1),
	mapping(Ts,Ns,D1,D2).

% copy(Term,Copy,AssocIn,AssocOut)
%
% Makes a copy of Term by changing all variables in it to new ones and
% building an association between original variables and the new ones.
% E.g. when Term = test(A,B,C), Copy = test(D,E,F) and an association between
% A and D, B and E and C and F is formed in AssocOut. AssocIn is input
% association.

copy(Term,Copy,D0,D1) :-
	var(Term),
	(   get_assoc(Term,D0,New)
	->  Copy = New,
	    D1 = D0
	;   put_assoc(Term,D0,Copy,D1)
	).
copy(Term,Copy,D0,D1) :-
	nonvar(Term),	% Term is a functor
	functor(Term,N,A),
	functor(Copy,N,A),	% Copy is new functor with the same name and arity as Term
	copy(A,Term,Copy,D0,D1).

% copy(Nb,Term,Copy,AssocIn,AssocOut)
%
% Makes a copy of the Nb arguments of Term by changing all variables in it to
% new ones and building an association between original variables and the new
% ones.
% See also copy/4

copy(0,_,_,D0,D0) :- !.
copy(1,T,C,D0,D1) :- !,
	arg(1,T,At1),
	arg(1,C,Ac1),
	copy(At1,Ac1,D0,D1).
copy(2,T,C,D0,D2) :- !,
	arg(1,T,At1),
	arg(1,C,Ac1),
	copy(At1,Ac1,D0,D1),
	arg(2,T,At2),
	arg(2,C,Ac2),
	copy(At2,Ac2,D1,D2).
copy(N,T,C,D0,D2) :-
	arg(N,T,At),
	arg(N,C,Ac),
	copy(At,Ac,D0,D1),
	N1 is N-1,
	copy(N1,T,C,D1,D2).