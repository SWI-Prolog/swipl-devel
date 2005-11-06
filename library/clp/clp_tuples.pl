/*  $Id$

    Part of SWI-Prolog

    Author:        Markus Triska
    E-mail:        triska@gmx.at
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, Markus Triska

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

%%%%%%%%%%%%%%% clp_tuples.pl - Symbolic constraints on tuples %%%%%%%%%%%%%%%

:- module(clp_tuples,
	[
		tuples_in/2
	]).

:- use_module(library('clp/bounds')).


trigger_exps([]).
trigger_exps([rel_tuple(Relation,Tuple)|Es]) :-
	( ground(Tuple) ->
		memberchk(Tuple, Relation)
	;
		relation_unifiable(Relation, Tuple, Us),
		Us \= [],
		( Us = [Single] ->
			Tuple = Single
		;
			tuple_domain(Tuple, Us)
		)
	),
	trigger_exps(Es).


attr_unify_hook(tuples(Exps), Y) :-
	trigger_exps(Exps),
	( var(Y) ->
		( get_attr(Y, clp_tuples, tuples(YExps)) ->
			append(Exps, YExps, NExps)
		;
			NExps = Exps
		),
		put_attr(Y, clp_tuples, tuples(NExps))
	;
		true
	).


tuples_in(Tuples, Relation) :-
	ground(Relation),
	bind_unique(Tuples, Relation),
	approx_tuples_domain(Tuples, Relation),
	tuples_freeze(Tuples, Relation).

bind_unique([], _).
bind_unique([Tuple|Ts], Relation) :-
	relation_unifiable(Relation, Tuple, Us),
	Us \= [],
	( Us = [Single] ->
		Single = Tuple
	;
		true
	),
	bind_unique(Ts, Relation).



   % The following predicates find component-wise extrema in the relation.

relation_mins_maxs(Relation, Mins, Maxs) :-
	Relation = [R|_],
	relation_mins_maxs(Relation, R, Mins, R, Maxs).

relation_mins_maxs([], Mins, Mins, Maxs, Maxs).
relation_mins_maxs([R|Rs], Mins0, Mins, Maxs0, Maxs) :-
	components_mins_maxs(R, Mins0, Mins1, Maxs0, Maxs1),
	relation_mins_maxs(Rs, Mins1, Mins, Maxs1, Maxs).

components_mins_maxs([], [], [], [], []).
components_mins_maxs([C|Cs], [Min0|Mins0], [Min|Mins], [Max0|Maxs0], [Max|Maxs]) :-
	Min is min(Min0, C),
	Max is max(Max0, C),
	components_mins_maxs(Cs, Mins0, Mins, Maxs0, Maxs).


   % Approximate the domains of each tuple variable for all tuples.
   % Heuristics: Take component-wise mins/maxs of the relation.

approx_tuples_domain(Tuples, Relation) :-
	relation_mins_maxs(Relation, Mins, Maxs),
	constrain_domains(Tuples, Mins, Maxs).

constrain_domains([], _, _).
constrain_domains([Tuple|Tuples], Mins, Maxs) :-
	constrain_domain(Tuple, Mins, Maxs),
	constrain_domains(Tuples, Mins, Maxs).

constrain_domain([], [], []).
constrain_domain([T|Ts], [Min|Mins], [Max|Maxs]) :-
	T in Min..Max,
	constrain_domain(Ts, Mins, Maxs).


tuple_domain(Tuple, Relation) :-
	relation_mins_maxs(Relation, Mins, Maxs),
	constrain_domain(Tuple, Mins, Maxs).


   % Set up the attributes for each tuple variable.
   % Attribute is a list of rel_tuple(Rel,Tuple) terms:
   %    Rel: the relation of which Tuple must be an element
   %    Tuple: the tuple to which this variable belongs
   % Note that the variable is itself part of the Tuple of its attribute.

tuple_freeze([],  _, _).
tuple_freeze([T|Ts], Tuple, Relation) :-
	( var(T) ->
		( get_attr(T, clp_tuples, tuples(Exps)) ->
			NExps = [rel_tuple(Relation,Tuple)|Exps]
		;
			NExps = [rel_tuple(Relation,Tuple)]
		),
		put_attr(T, clp_tuples, tuples(NExps))
	;
		true
	),
	tuple_freeze(Ts, Tuple, Relation).

tuples_freeze([], _).
tuples_freeze([Tuple|Tuples], Relation) :-
	tuple_freeze(Tuple, Tuple, Relation),
	tuples_freeze(Tuples, Relation).


   % Find all tuples in the relation that can unify with Tuple,
   % ignoring constraints.

relation_unifiable([], _, []).
relation_unifiable([R|Rs], Tuple, Us) :-
	( unifiable(R, Tuple, _) ->
		Us = [R|Rest]
	;
		Us = Rest
	),
	relation_unifiable(Rs, Tuple, Rest).

attr_portray_hook(rel_tuple(_Rel,_), _) :-
	write(clp_tuples).
