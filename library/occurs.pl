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

:- module(occurs,
	  [ contains_term/2,		% +SubTerm, +Term
	    contains_var/2,		% +SubTerm, +Term
	    free_of_term/2,		% +SubTerm, +Term
	    free_of_var/2,		% +SubTerm, +Term
	    occurrences_of_term/3,	% +SubTerm, +Term, ?Tally
	    occurrences_of_var/3,	% +SubTerm, +Term, ?Tally
	    sub_term/2,			% -SubTerm, +Term
	    sub_var/2			% -SubTerm, +Term (SWI extra)
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  is  a  SWI-Prolog  implementation  of  the  corresponding  Quintus
library, based on the generalised arg/3 predicate of SWI-Prolog.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	contains_term(+Sub, +Term)
%
%	Succeeds if Sub is contained in Term (=, deterministically)

contains_term(X, X) :- !.
contains_term(X, Term) :-
	compound(Term),
	arg(_, Term, Arg),
	contains_term(X, Arg).


%	contains_var(+Sub, +Term)
%
%	Succeeds if Sub is contained in Term (==, deterministically)

contains_var(X0, X1) :-
	X0 == X1, !.
contains_var(X, Term) :-
	compound(Term),
	arg(_, Term, Arg),
	contains_var(X, Arg).

%	free_of_term(+Sub, +Term)
%
%	Succeeds of Sub does not unify to any subterm of Term

free_of_term(Sub, Term) :-
	\+ contains_term(Sub, Term).

%	free_of_var(+Sub, +Term)
%
%	Succeeds of Sub is not equal (==) to any subterm of Term

free_of_var(Sub, Term) :-
	\+ contains_var(Sub, Term).

%	occurrences_of_term(+SubTerm, +Term, ?Count)
%
%	Count the number of SubTerms in Term

occurrences_of_term(Sub, Term, Count) :-
	count(sub_term(Sub, Term), Count).

%	occurrences_of_var(+SubTerm, +Term, ?Count)
%
%	Count the number of SubTerms in Term

occurrences_of_var(Sub, Term, Count) :-
	count(sub_var(Sub, Term), Count).

%	sub_term(-Sub, +Term)
%
%	Generates (on backtracking) all subterms of Term.

sub_term(X, X).
sub_term(X, Term) :-
	compound(Term),
	arg(_, Term, Arg),
	sub_term(X, Arg).

%	sub_var(-Sub, +Term)
%
%	Generates (on backtracking) all subterms (==) of Term.

sub_var(X0, X1) :-
	X0 == X1.
sub_var(X, Term) :-
	compound(Term),
	arg(_, Term, Arg),
	sub_var(X, Arg).


		 /*******************************
		 *		UTIL		*
		 *******************************/

%	count(+Goal, -Count)
%
%	Count number of times Goal succeeds.

count(Goal, Count) :-
	flag('$occurs_count', Old, 0),
	(   Goal,
	    flag('$occurs_count', X, X+1),
	    fail
	;   flag('$occurs_count', Count0, Old)
	),
	Count0 = Count.

