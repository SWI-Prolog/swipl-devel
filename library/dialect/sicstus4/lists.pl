/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020-2021, SWI-Prolog Solutions b.v.
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
	    rev/2,			% +List, ?Reversed
	    shorter_list/2,		% ?Short, ?Long
	    append_length/4,		% ?Prefix, ?Suffix, ?List, ?Length
	    append_length/3,		% ?Suffix, ?List, ?Length
	    prefix_length/3,		% ?List, ?Prefix, ?Length
	    proper_prefix_length/3,	% ?List, ?Prefix, ?Length
	    suffix_length/3,		% ?List, ?Suffix, ?Length
	    proper_suffix_length/3,	% ?List, ?Suffix, ?Length
	    sublist/5,			% +Whole, ?Part, ?Before, ?Length, ?After
	    sublist/4,			% +Whole, ?Part, ?Before, ?Length
	    sublist/3,			% +Whole, ?Part, ?Before
	    cons/3,			% ?Head, ?Tail, ?List
	    last/3,			% ?Fore, ?Last, ?List
	    head/2,			% ?List, ?Head
	    tail/2,			% ?List, ?Tail
	    prefix/2,			% ?List, ?Prefix
	    proper_prefix/2,		% ?List, ?Prefix
	    suffix/2,			% ?List, ?Suffix
	    proper_suffix/2,		% ?List, ?Suffix
	    subseq/3,			% ?Sequence, ?SubSequence, ?Complement
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
	      % SWI lists:list_to_set/2 actually behaves
	      % like SICStus lists:remove_dups/2
	      % and not like SICStus sets:list_to_set/2.
	      list_to_set/2 as remove_dups,
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
	      max_member/2,
	      min_member/2,
	      max_member/3,
	      min_member/3,
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
:- reexport('../sicstus/lists', [same_length/3]).
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
	* rotate_list/[2,3]
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


%%	rev(+List, ?Reversed) is semidet.
%
%	Same as reverse/2, but List must be a proper list.

rev(List, Reversed) :- rev_(List, [], Reversed).
rev_([], Reversed, Reversed).
rev_([Head|Tail], RevTail, Reversed) :-
	rev_(Tail, [Head|RevTail], Reversed).


%%	shorter_list(?Short, ?Long) is nondet.
%
%	True if Short is a shorter list than Long. The lists' contents
%	are insignificant, only the lengths matter. Mode -Short, +Long
%	can be used to enumerate list skeletons shorter than Long.

shorter_list([], [_|_]).
shorter_list([_|ShortTail], [_|LongTail]) :-
	shorter_list(ShortTail, LongTail).


% TODO The *_length predicates can probably be implemented more efficiently.

append_length(Prefix, Suffix, List, Length) :-
	append(Prefix, Suffix, List),
	length(Prefix, Length).

append_length(Suffix, List, Length) :-
	append_length(_, Suffix, List, Length).

prefix_length(List, Prefix, Length) :-
	prefix(List, Prefix),
	length(Prefix, Length).

proper_prefix_length(List, Prefix, Length) :-
	proper_prefix(List, Prefix),
	length(Prefix, Length).

suffix_length(List, Suffix, Length) :-
	suffix(List, Suffix),
	length(Suffix, Length).

proper_suffix_length(List, Suffix, Length) :-
	proper_suffix(List, Suffix),
	length(Suffix, Length).


sublist(Whole, Part, Before, Length, After) :-
	append(Prefix, Tail, Whole),
	append(Part, Suffix, Tail),
	length(Prefix, Before),
	length(Part, Length),
	length(Suffix, After).

sublist(Whole, Part, Before, Length) :-
	sublist(Whole, Part, Before, Length, _).

sublist(Whole, Part, Before) :-
	sublist(Whole, Part, Before, _, _).


cons(Head, Tail, [Head|Tail]).
last(Fore, Last, List) :- append(Fore, [Last], List).
head([Head|_], Head).
tail([_|Tail], Tail).


%%	prefix(?List, ?Prefix) is nondet.
%
%	True if Prefix is a prefix of List. Not the same as prefix/2
%	in SICStus 3 or SWI - the arguments are reversed!

prefix(List, Prefix) :-
	append(Prefix, _, List).

%%	proper_prefix(?List, ?Prefix) is nondet.
%
%	True if Prefix is a prefix of List, but is not List itself.

proper_prefix(List, Prefix) :-
	prefix(List, Prefix),
	Prefix \== List.

%%	suffix(?List, ?Prefix) is nondet.
%
%	True if Suffix is a suffix of List. Not the same as suffix/2
%	in SICStus 3 - the arguments are reversed!

suffix(List, List).
suffix([_|Tail], Suffix) :-
	suffix(Tail, Suffix).

%%	proper_suffix(?List, ?Prefix) is nondet.
%
%	True if Suffix is a suffix of List, but is not List itself.

proper_suffix([_|Tail], Suffix) :-
	suffix(Tail, Suffix).


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
