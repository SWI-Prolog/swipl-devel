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
		tuples_in/2,
		tuples_in/3
	]).

:- use_module(library('clp/bounds')).
:- use_module(library('clp/clp_events')).

   % Entry point for notifications by bounds.pl Similar to attr_unify_hook/2.

notify_hook(T) :-
	( var(T) ->
		get_attr(T, clp_tuples, rel_tuple(Relation,Tuple)),
		relation_unifiable_(Relation, Tuple, Us),
		Us \= [],
		( Us = [Single] ->
			Single = Tuple
		; Us == Relation ->
			true
		;
			put_attr(T, clp_tuples, rel_tuple(Us,Tuple)),
			update_tuplevars(Tuple, Tuple, Us),
			tuple_domain(Tuple, Us)
		)
	;
		true
	).



   % A tuple variable was unified with Y. Find the tuples in relation that
   % the new resulting tuple can still be unified with and update all other
   % tuple variables accordingly.

attr_unify_hook(rel_tuple(Relation,Tuple), Y) :-
	%format("val: ~w tuple: ~w\n", [Y, Tuple]),
	( ground(Tuple) ->
		memberchk(Tuple, Relation)
	;
		relation_unifiable_(Relation, Tuple, Us),
		Us \= [],
		( Us = [Single] ->
			Tuple = Single
		;
			( var(Y) ->
				put_attr(Y, clp_tuples, rel_tuple(Us,Tuple))
			;
				true
			),
			( Us == Relation ->
				true
			;
				update_tuplevars(Tuple, Tuple, Us),
				tuple_domain(Tuple, Us)
			)
		)
	).

update_tuplevars([], _, _).
update_tuplevars([T|Ts], Tuple, Relation) :-
	( var(T) ->
		put_attr(T, clp_tuples, rel_tuple(Relation, Tuple))
	;
		true
	),
	update_tuplevars(Ts, Tuple, Relation).

tuples_in(Tuples, Relation) :-
	tuples_in(Tuples, Relation, []).

tuples_in(Tuples, Relation, Opts) :-
	ground(Relation),
	bind_unique(Tuples, Relation),
	( memberchk(propagate(full), Opts) ->
		full_tuples_constrain(Tuples, Relation, Opts)
	;
		approx_tuples_domain(Tuples, Relation),
		tuples_freeze(Tuples, Relation, Opts)
	).

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


full_tuples_constrain([], _, _).
full_tuples_constrain([Tuple|Tuples], Relation, Opts) :-
	relation_unifiable_(Relation, Tuple, Matching),
	Matching \= [],
	( Matching = [Single] ->
		Single = Tuple
	;
		tuple_domain(Tuple, Matching),
		tuple_freeze(Tuple, Tuple, Matching, Opts)
	),
	full_tuples_constrain(Tuples, Relation, Opts).


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
   % Attributes are rel_tuple(Rel, Tuple) terms:
   %    Rel: the relation of which Tuple must be an element
   %    Tuple: the tuple to which this variable belongs
   % Note that the variable is itself part of the Tuple of its attribute.

tuple_freeze([],  _, _, _Opts).
tuple_freeze([T|Ts], Tuple, Relation, Opts) :-
	( var(T) ->
		put_attr(T, clp_tuples, rel_tuple(Relation,Tuple)),
		( memberchk(subscribe(bounds), Opts) ->
			subscribe(T, bounds, bounds, clp_tuples:notify_hook(T))
		;
			true
		)
	;
		true
	),
	tuple_freeze(Ts, Tuple, Relation, Opts).

tuples_freeze([], _, _).
tuples_freeze([Tuple|Tuples], Relation, Opts) :-
	tuple_freeze(Tuple, Tuple, Relation, Opts),
	tuples_freeze(Tuples, Relation, Opts).



many_unifiable([R|Rs], Tuple, Count0) :-
	( \+(\+ R = Tuple) ->
		Count1 is Count0 + 1,
		( Count1 >= 3 ->
			true
		;
			many_unifiable(Rs, Tuple, Count1)
		)
	;
		many_unifiable(Rs, Tuple, Count0)
	).

   % Like relation_unifiable_, but resorts to not taking it too seriously
   % if there are "many" tuples that unify (in which case nothing at all
   % is filtered).
 
relation_unifiable(Relation, Tuple, Us) :-
	( many_unifiable(Relation, Tuple, 0) ->
		Us = Relation
	;
		relation_unifiable_(Relation, Tuple, Us)
	).

   % Find all tuples in the relation that can unify with Tuple.
   % Note that we can not use unifiable/3 because that does not take into
   % account attributed variables, most notably not constraints set up
   % by bounds.pl. No bindings must be generated, hence \+\+.

relation_unifiable_([], _, []).
relation_unifiable_([R|Rs], Tuple, Us) :-
	( \+(\+ R = Tuple) ->
		Us = [R|Rest],
		relation_unifiable_(Rs, Tuple, Rest)
	;
		relation_unifiable_(Rs, Tuple, Us)
	).

attr_portray_hook(rel_tuple(_Rel,_), _) :-
	write(clp_tuples).
