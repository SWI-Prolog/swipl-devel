/*  $Id$

    Part of CPL(R) (Constraint Logic Programming over Reals)

    Author:        Leslie De Koninck
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2004, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is part of Leslie De Koninck's master thesis, supervised
    by Bart Demoen and daily advisor Tom Schrijvers.  It is based on CLP(Q,R)
    by Christian Holzbaur for SICStus Prolog and distributed under the
    license details below with permission from all mentioned authors.

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


:- module(geler_r,
	[
		geler/2,
		project_nonlin/3,
		collect_nonlin/3
	]).

%:- attribute goals/1, all_nonlin/1.

attribute_goal(X,Goals) :-
	get_attr(X,geler_r,g(goals(Gs),_)),
	nonexhausted(Gs,Goals,[]),
	Goals = [_|_].
attribute_goal(X,Conj) :- 
	get_attr(X,geler_r,g(_,all_nonlin(Goals))),
	l2conj(Goals,Conj).

% l2conj(List,Conj)
%
% turns a List into a conjunction of the form (El,Conj) where Conj 
% is of the same form recursively and El is an element of the list

l2conj([X|Xs],Conj) :-
	(
		Xs = [],
		Conj = X
	;
		Xs = [_|_],
		Conj = (X,Xc),
		l2conj(Xs,Xc)
	).

% nonexhausted(Goals,OutList,OutListTail) 
%
% removes the goals that have already run from Goals
% and puts the result in the difference list OutList 

nonexhausted(run(Mutex,G)) -->  
	(
	  {var(Mutex)} ->

		[G]
	;
		[]
	).
nonexhausted((A,B)) -->
	nonexhausted(A),
	nonexhausted(B).

attr_unify_hook((goals(Gx),_),Y) :-
	!,
	(
		var(Y),
		(
		  % possibly mutual goals. these need to be run. other goals are run
		  % as well to remove redundant goals.
		  get_attr(Y,geler_r,g(goals(Gy),Other)) ->
		
			Later = [Gx,Gy],
			(
			  Other = n ->
			
				del_attr(Y,geler_r)
			;
				put_attr(Y,geler_r,g(n,Other))
			)
		;
		  % no goals in Y, so no mutual goals of X and Y, store goals of X in Y
		  % no need to run any goal.
		  get_attr(Y,geler_r,g(n,Other)) ->
		 
			Later = [],
			put_attr(Y,geler_r,g(goals(Gx),Other))
		;
			Later = [],
			put_attr(Y,geler_r,g(goals(Gx),n))
		)
	;
		nonvar(Y),
		Later = [Gx]
	),
	call_list(Later).
attr_unify_hook(_,_). % no goals in X

% call_list(List)
%
% Calls all the goals in List.

call_list([]).
call_list([G|Gs]) :-
	call(G),
	call_list(Gs).




%
% called from project.pl
%
project_nonlin(_,Cvas,Reachable) :-
	collect_nonlin(Cvas,L,[]),
	sort(L,Ls),
	term_variables(Ls,Reachable).
	%put_attr(_,all_nonlin(Ls)).


collect_nonlin([]) --> [].
collect_nonlin([X|Xs]) -->
	(
	  {get_attr(X,geler_r,g(goals(Gx),_))} ->

		trans(Gx),
		collect_nonlin(Xs)
	;
		collect_nonlin(Xs)
	).

% trans(Goals,OutList,OutListTail)
%
% transforms the goals (of the form run(Mutex,Goal)
% that are in Goals (in the conjunction form, see also l2conj)
% that have not been run (var(Mutex) into a readable output format
% and notes that they're done (Mutex = done). Because of the Mutex
% variable, each goal is only added once (so not for each variable).

trans((A,B)) -->
	trans(A),
	trans(B).
trans(run(Mutex,Gs)) --> 
	(
	  {var(Mutex)} ->
		{Mutex = done},
		transg(Gs)
	;
		[]
	).

transg((A,B)) -->
	!,
	transg(A),
	transg(B).
transg(M:G) --> 
	!,
	M:transg(G).
transg(G) --> [G].

% run(Mutex,G)
%
% Calls goal G if it has not yet run (Mutex is still variable)
% and stores that it has run (Mutex = done). This is done so
% that when X = Y and X and Y are in the same goal, that goal
% is called only once.

run(Mutex,_) :- nonvar(Mutex).
run(Mutex,G) :- 
	var(Mutex),
	Mutex = done,
	call(G).

% geler(Vars,Goal)
%
% called by nf.pl when an unsolvable non-linear expression is found
% Vars contain the variables of the expression, Goal contains the predicate of nf.pl to be called when 
% the variables are bound.

geler(Vars,Goal) :-
	attach(Vars,run(_Mutex,Goal)).	
	% one goal gets the same mutex on every var, so it is run only once

% attach(Vars,Goal)
%
% attaches a new goal to be awoken when the variables get bounded.
% when the old value of the attribute goals = OldGoal, then the new value = (Goal,OldGoal)

attach([],_).
attach([V|Vs],Goal) :-
	(
	  var(V),
	  get_attr(V,geler_r,g(goals(Gv),Other)) ->
	
		put_attr(V,geler_r,g(goals((Goal,Gv)),Other))
	;
	  get_attr(V,geler_r,(n,Other)) ->

		put_attr(V,geler_r,g(goals(Goal),Other))
	;
		put_attr(V,geler_r,g(goals(Goal),n))
	),
	attach(Vs,Goal). 
