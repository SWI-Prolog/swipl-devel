/*  $Id$

    Part of CLP(Q) (Constraint Logic Programming over Rationals)

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

:- module(geler,
	[
	    geler/3,
	    project_nonlin/3,
	    collect_nonlin/3
	]).

% l2conj(List,Conj)
%
% turns a List into a conjunction of the form (El,Conj) where Conj
% is of the same form recursively and El is an element of the list

l2conj([X|Xs],Conj) :-
	(   X = [],
	    Conj = X
	;   Xs = [_|_],
	    Conj = (X,Xc),
	    l2conj(Xs,Xc)
	).

% nonexhausted(Goals,OutList,OutListTail)
%
% removes the goals that have already run from Goals
% and puts the result in the difference list OutList

nonexhausted(run(Mutex,G)) -->
	(   { var(Mutex) }
	->  [G]
	;   []
	).
nonexhausted((A,B)) -->
	nonexhausted(A),
	nonexhausted(B).

attr_unify_hook(g(CLP,goals(Gx),_),Y) :-
	!,
	(   var(Y),
	    (   get_attr(Y,geler,g(A,B,C))
	    ->  ignore((CLP \== A,throw(error(permission_error(
		    'apply CLP(Q) constraints on','CLP(R) variable',Y),
		    context(_))))),
		(   % possibly mutual goals. these need to be run.
		    % other goals are run as well to remove redundant goals.
		    B = goals(Gy)
		->  Later = [Gx,Gy],
		    (   C = n
		    ->  del_attr(Y,geler)
		    ;   put_attr(Y,geler,g(CLP,n,C))
		    )
		;   % no goals in Y, so no mutual goals of X and Y, store
		    % goals of X in Y 
		    % no need to run any goal.
		    Later = [],
		    put_attr(Y,geler,g(CLP,goals(Gx),C))
		)
	    ;	Later = [],
		put_attr(Y,geler,g(CLP,goals(Gx),n))
	    )
	;   nonvar(Y),
	    Later = [Gx]
	),
	maplist(call,Later).
attr_unify_hook(_,_). % no goals in X

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
	(   { get_attr(X,geler,g(_,goals(Gx),_)) }
	->  trans(Gx),
	    collect_nonlin(Xs)
	;   collect_nonlin(Xs)
	).

% trans(Goals,OutList,OutListTail)
%
% transforms the goals (of the form run(Mutex,Goal)
% that are in Goals (in the conjunction form, see also l2conj)
% that have not been run (Mutex = variable) into a readable output format
% and notes that they're done (Mutex = 'done'). Because of the Mutex
% variable, each goal is only added once (so not for each variable).

trans((A,B)) -->
	trans(A),
	trans(B).
trans(run(Mutex,Gs)) -->
	(   { var(Mutex) }
	->  { Mutex = done },
	    transg(Gs)
	;   []
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
% Vars contain the variables of the expression, Goal contains the predicate of
% nf.pl to be called when the variables are bound.

geler(CLP,Vars,Goal) :-
	attach(Vars,CLP,run(_Mutex,Goal)).
	% one goal gets the same mutex on every var, so it is run only once

% attach(Vars,Goal)
%
% attaches a new goal to be awoken when the variables get bounded.
% when the old value of the attribute goals = OldGoal, then the new value =
% (Goal,OldGoal)

attach([],_,_).
attach([V|Vs],CLP,Goal) :-
	var(V),
	(   get_attr(V,geler,g(A,B,C))
	->  (   CLP \== A
	    ->  throw(error(permission_error('apply CLP(Q) constraints on',
		    'CLP(R) variable',V),context(_)))
	    ;   (   B = goals(Goals)
	        ->  put_attr(V,geler,g(A,goals((Goal,Goals)),C))
	        ;   put_attr(V,geler,g(A,goals(Goal),C))
	        )
	    )
	;   put_attr(V,geler,g(CLP,goals(Goal),n))
	),	
	attach(Vs,CLP,Goal).