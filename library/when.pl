/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers, K.U.Leuven
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U.Leuven

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the when/2 co-routine.
%	
%	when(+Condition, :Goal)
%
%		Condition should be one of 
%			?=(X,Y)
%			nonvar(X)
%			ground(X)
%			(Condition,Condition)
%			(Condition;Condition)
%
%	Author: 	Tom Schrijvers, K.U.Leuven
% 	E-mail: 	Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% History:
% 
%	Apr 9, 2004
%	* JW: Supressed debugging this module
%	* JW: Made when/2 module-aware.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Simple implementation. Does not clean up redundant attributes.
% Now deals with cyclic terms.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(when,
	  [ when/2			% +Condition, :Goal
	  ]).
:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
	when/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
when(Condition, Goal) :-
	'$strip_module'(Goal, M, G),
	trigger(Condition, M:G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trigger(nonvar(X),Goal) :-
	trigger_nonvar(X,Goal).

trigger(ground(X),Goal) :-
	trigger_ground(X,Goal).

trigger(?=(X,Y),Goal) :-
	trigger_determined(X,Y,Goal).

trigger((G1,G2),Goal) :-
	trigger_conj(G1,G2,Goal).

trigger((G1;G2),Goal) :-
	trigger_disj(G1,G2,Goal).

trigger_nonvar(X,Goal) :-
	( nonvar(X) ->
		call(Goal)
	;
		suspend(X,trigger_nonvar(X,Goal))
	).

trigger_ground(X,Goal) :-
	term_variables(X,Vs),
	( Vs = [H|_] ->
		suspend(H,trigger_ground(Vs,Goal))
	;
		call(Goal)
	).

trigger_determined(X,Y,Goal) :-
	unifyable(X,Y,Unifier),
	!,
	( Unifier == [] ->
		call(Goal)
	;
		put_attr(Det,when,det(trigger_determined(X,Y,Goal))),
		suspend_list(Unifier,wake_det(Det))
	).
	
trigger_determined(_,_,Goal) :-
	call(Goal).


wake_det(Det) :-
	( var(Det) ->
		get_attr(Det,when,Attr),
		del_attr(Det,when),
		Det = (-),
		Attr = det(Goal),
		call(Goal)
	;
		true
	).

trigger_conj(G1,G2,Goal) :-
	trigger(G1,trigger(G2,Goal)).

trigger_disj(G1,G2,Goal) :-
	trigger(G1,check_disj(Disj,Goal)),
	trigger(G2,check_disj(Disj,Goal)).

check_disj(Disj,Goal) :-
	( var(Disj) ->
		Disj = (-),
		call(Goal)
	;
		true
	).

suspend_list([],_Goal).
suspend_list([V=_|Unifier],Goal) :-
	suspend(V,Goal),
	suspend_list(Unifier,Goal).

suspend(V,Goal) :-
	( get_attr(V,when,List) ->
		put_attr(V,when,[Goal|List])
	;
		put_attr(V,when,[Goal])
	).

attr_unify_hook(List,Other) :-
	List = [_|_],
	( get_attr(Other,when,List2) ->
		del_attr(Other,when),
		call_list(List),
		call_list(List2)
	;
		call_list(List) 
	).	

call_list([]).
call_list([G|Gs]) :-
	call(G),
	call_list(Gs).
