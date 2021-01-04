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

:- module(sicstus4_lists,
	  [ keys_and_values/3,		% ?Pairs, ?Keys, ?Values
	    subseq0/2,			% +Sequence, ?SubSequence
	    subseq1/2,			% +Sequence, ?SubSequence
	    scanlist/4,			% :Pred, ?Xs, ?V1, ?V
	    scanlist/5,			% :Pred, ?Xs, ?Ys, ?V1, ?V
	    scanlist/6,			% :Pred, ?Xs, ?Ys, ?Zs, ?V1, ?V
	    
	    % is_list/1 is built-in on SWI.
	    % We re-export it here to avoid warnings
	    % when SICStus code explicitly imports it from library(lists).
	    is_list/1			% +Term
	  ]).
:- reexport('../../lists',
	    [ select/3,
	      selectchk/3,
	      append/2,
	      delete/3,
	      last/2,
	      nextto/3,
	      nth1/3,
	      nth1/4,
	      nth0/3,
	      nth0/4,
	      permutation/2,
	      proper_length/2,
	      reverse/2,
	      same_length/2,
	      select/4,
	      selectchk/4,
	      sum_list/2 as sumlist,
	      prefix/2,
	      max_member/2,
	      min_member/2,
	      clumped/2
	    ]).
:- reexport('../../apply',
	    [ maplist/2,
	      maplist/3,
	      maplist/4,
	      convlist/3,
	      exclude/3,
	      include/3,
	      partition/5
	    ]).
:- reexport('../../clp/clpfd', [transpose/2]).
:- use_module(library(pairs), [pairs_keys_values/3]).

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(lists, sicstus4_lists).

/** <module> SICStus 4-compatible library(lists).

@tbd	This library is incomplete.
	As of SICStus 4.6.0, the following predicates are missing:

	* append/5
	* correspond/4
	* delete/4
	* one_longer/2
	* perm/2
	* perm2/4
	* remove_dups/2
	* rev/2
	* same_length/3
	* shorter_list/2
	* append_length/[3,4]
	* prefix_length/3
	* proper_prefix_length/3
	* suffix_length/3
	* proper_suffix_length/3
	* rotate_list/[2,3]
	* sublist/[3,4,5]
	* cons/3
	* last/3
	* head/2
	* tail/2
	* proper_prefix/2
	* suffix/2
	* proper_suffix/2
	* segment/2
	* proper_segment/2
	* cumlist/[4,5,6]
	* map_product/4
	* some/[2,3,4]
	* somechk/[2,3,4]
	* exclude/[4,5]
	* include/[4,5]
	* group/[3,4,5]
	* ordered/[1,2]
	* max_member/3
	* min_member/3
	* select_min/[3,4]
	* select_max/[3,4]
	* increasing_prefix/[3,4]
	* decreasing_prefix/[3,4]
	* clumps/2
	* keyclumps/2
	* keyclumped/2

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dlists.html
*/

keys_and_values(Pairs, Keys, Values) :-
	pairs_keys_values(Pairs, Keys, Values).


%%	scanlist(:Pred, ?Xs, ?V1, ?V) is nondet.
%%	scanlist(:Pred, ?Xs, ?Ys, ?V1, ?V) is nondet.
%%	scanlist(:Pred, ?Xs, ?Ys, ?Zs, ?V1, ?V) is nondet.
%
%	Same as foldl/[4,5,6].
%
%	@compat SICStus 4

:- meta_predicate scanlist(3, ?, ?, ?).
scanlist(Pred, Xs, V1, V) :- foldl(Pred, Xs, V1, V).
:- meta_predicate scanlist(4, ?, ?, ?, ?).
scanlist(Pred, Xs, Ys, V1, V) :- foldl(Pred, Xs, Ys, V1, V).
:- meta_predicate scanlist(5, ?, ?, ?, ?, ?).
scanlist(Pred, Xs, Ys, Zs, V1, V) :- foldl(Pred, Xs, Ys, Zs, V1, V).


subseq(Sequence, [], Sequence).
subseq([Head|Tail], [Head|SubTail], Complement) :-
	subseq(Tail, SubTail, Complement).
subseq([Head|Tail], SubSequence, [Head|Complement]) :-
	subseq(Tail, SubSequence, Complement).


subseq0(Sequence, SubSequence) :- subseq(Sequence, SubSequence, _).
subseq1(Sequence, SubSequence) :-
	subseq(Sequence, SubSequence, Complement),
	Complement \== [].
