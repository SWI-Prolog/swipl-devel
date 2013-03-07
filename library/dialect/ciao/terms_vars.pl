/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

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

:- module(terms_vars, [varset/2, intersect_vars/3, member_var/2, diff_vars/3,
		       varsbag/3, term_variables/2],
	  [assertions]).

varset(Term, List) :- term_variables(Term, List).

%-------------------------------------------------------------------------

:- pred varset_in_args(T, LL) : nonvar(T) => list(LL, list(var)) # "Each
   list of @var{LL} contains the variables of an argument of @var{T},
   for each argument, and in left to right order.".

varset_in_args(Term, Xss) :-
	Term =.. [_|Args],
	vars_in_args(Args, Xss).

vars_in_args([],         []).
vars_in_args([Arg|Rest], [Arg_list|Rest_list]) :-
	varset(Arg, Arg_list),
	vars_in_args(Rest, Rest_list).

intersect_vars([],     _,  []).
intersect_vars([X|S1], S2, S) :-
	( member_var(S2, X) ->
	    S = [X|SList] ;
	    S = SList ),
	intersect_vars(S1, S2, SList).

diff_vars([],     _L, []).
diff_vars([H|L1], L2, L3) :-
	member_var(L2, H),
	!,
	diff_vars(L1, L2, L3).
diff_vars([H|L1], L2, [H|L3]) :-
	diff_vars(L1, L2, L3).

% member_var([],       _) :- fail.
member_var([E|List], Ele) :-
	E == Ele -> true ; member_var(List, Ele).

%%      varsbag(+Term, -Vs, ?Xs)
%
%       Vs is the list of all the variables in Term ordered as they
%       appear in Term right-to-left depth-first (including
%       duplicates) plus Xs.

varsbag(X, Vars, Tail) :-
	var(X), !,
	Vars = [X|Tail].
varsbag([H|T], Vars, Tail) :- !,
	varsbag(H, Vars,  Tail0),
	varsbag(T, Tail0, Tail).
varsbag(Term, Vars, Tail) :-
	functor(Term, _, A),
	go_inside(A, Term, Vars, Tail).

go_inside(0, _, Tail, Tail) :- !.
go_inside(N, T, Bag,  Tail) :-
	Nth is N-1,
	arg(N, T, ARG),
	varsbag(ARG, Bag, Tail0),
	go_inside(Nth, T, Tail0, Tail).
