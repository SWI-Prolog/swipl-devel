/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(sicstus4_terms,
	  [ term_variables_set/2,	% @Term, -Variables
	    subsumeschk/2,		% +Generic, @Specific
	    term_order/3,		% @X, @Y, -R
	    same_functor/2,		% ?T1, ?T2
	    same_functor/3,		% ?T1, ?T2, ?N
	    same_functor/4		% ?T1, ?T2, ?F, ?N
	  ]).
:- reexport('../sicstus/terms').

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(terms, sicstus4_terms).

/** <module> SICStus 4-compatible library(terms).

@tbd	This library is incomplete.
	As of SICStus 4.6.0, the following predicates are missing:

	* term_hash/3
	* contains_term/2
	* free_of_term/2
	* occurrences_of_term/2
	* contains_var/2
	* free_of_var/2
	* occurrences_of_var/2
	* sub_term/2
	* depth_bound/2
	* length_bound/2
	* size_bound/2
	* term_depth/2
	* term_size/2

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dterms.html
*/

%%	term_variables_set(@Term, -Variables) is det.
%
%	Same as term_variables_bag/2, but Variables is an ordered set.

term_variables_set(Term, Variables) :-
	term_variables(Term, VariablesBag),
	sort(VariablesBag, Variables).

%%	subsumeschk(+Generic, @Specific) is semidet.
%
%	SICStus 4 name of subsumes_chk/2.
%
%	@deprecated Replace by subsumes_term/2.

subsumeschk(Generic, Specific) :- subsumes_chk(Generic, Specific).

%%	term_order(@X, @Y, -R) is det.
%
%	Same as compare/3, except for the order of arguments.
%
%	@deprecated Use the standard compare/3 instead.

term_order(X, Y, R) :- compare(R, X, Y).

%%	same_functor(?T1, ?T2) is semidet.
%%	same_functor(?T1, ?T2, ?N) is semidet.
%%	same_functor(?T1, ?T2, ?F, ?N) is semidet.
%
%	True if T1 and T2 have the same functor F and arity N.
%	Any unbound arguments will be computed from the bound arguments if possible.
%	At least one of T1, T2, or both F and N must be bound,
%	otherwise an error is thrown.

same_functor(T1, T2) :- same_functor(T1, T2, _, _).
same_functor(T1, T2, N) :- same_functor(T1, T2, _, N).
same_functor(T1, T2, F, N) :-
	nonvar(T1),
	!,
	functor(T1, F, N),
	functor(T2, F, N).
same_functor(T1, T2, F, N) :-
	functor(T2, F, N),
	functor(T1, F, N).
