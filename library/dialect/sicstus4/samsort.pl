/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(samsort,
	  [ samsort/2,			% +RawList, -Sorted
	    samsort/3,			% :Order, +RawList, -SortedList
	    samkeysort/2		% +RawList, -Sorted
	  ]).
:- reexport('../../backcomp', [merge/3]).

/** <module> SICStus 4 library(samsort).

@tbd	This library is incomplete.
	As of SICStus 4.6.0, the following predicates are missing:

	* merge/4
	* keymerge/3

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dsamsort.html
*/

%!	samsort(+RawList, -Sorted) is det.
%
%	Same as msort/2.
%
%	@compat SICStus 4

samsort(RawList, Sorted) :- msort(RawList, Sorted).

:- meta_predicate samsort_3_key(2, -, +, +).
samsort_3_key(OrderPred, Delta, E1, E2) :-
	% Intentionally never set Delta = (=),
	% because predsort removes duplicates
	% (i. e. if two items compare equal, one is removed),
	% but SICStus samsort doesn't.
	call(OrderPred, E1, E2) -> Delta = (<) ; Delta = (>).

%!	samsort(:Order, +RawList, -Sorted) is det.
%
%	Similar to predsort/3, but the predicate Order is used to determine the
%	order of terms. Order is called with two arguments, and if it succeeds,
%	the first argument will be sorted before the second argument.
%	Duplicates are not removed.
%
%	@compat SICStus 4

:- meta_predicate samsort(2, +, -).
samsort(Order, RawList, SortedList) :-
	predsort(samsort_3_key(Order), RawList, SortedList).

%!	samkeysort(+RawList, -Sorted) is det.
%
%	Same as keysort/2.
%
%	@compat SICStus 4

samkeysort(RawList, Sorted) :- keysort(RawList, Sorted).
