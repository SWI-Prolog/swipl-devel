/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
                         CWI, Amsterdam
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

:- module(dcg_high_order,
          [ sequence//2,                % :Element, ?List
            sequence//3,                % :Element, :Sep, ?List
            sequence//5,                % :Start, :Element, :Sep, :End, ?List
            optional//2,                % :Match, :Otherwise
            foreach//2,                 % :Generator, :Element
            foreach//3                  % :Generator, :Element, :Sep
          ]).
:- use_module(library(ordsets)).

:- meta_predicate
    sequence(3,?,?,?),
    sequence(3,//,?,?,?),
    sequence(//,3,//,//,?,?,?),
    optional(//, //, ?, ?),
    foreach(0,//,?,?),
    foreach(0,//,//,?,?).

/** <module> High order grammar operations

This library provides facilities  comparable   maplist/3,  ignore/1  and
foreach/2 for DCGs.

__STATUS__:  This  library   is   experimental.    The   interface   and
implementation may change based on feedback. Please send feedback to the
mailinglist or the issue page of the `swipl-devel.git` repository.
*/

%!  sequence(:Element, ?List)// is nondet.
%
%   Match  or  generate  a  sequence  of   Element.  This  predicate  is
%   deterministic  if  List  is  fully    instantiated  and  Element  is
%   deterministic. When parsing, this predicate is _gready_ and does not
%   prune choice points. For example:
%
%       ?- phrase(sequence(digit, Digits), `123a`, L).
%       Digits = "123",
%       L = [97] ;
%       Digits = [49, 50],
%       L = [51, 97] ;
%       ...

sequence(OnElem, List) -->
    sequence_(List, OnElem).

sequence_([H|T], P) -->
    call(P, H),
    sequence_(T, P).
sequence_([], _) -->
    [].

%!  sequence(:Element, :Sep, ?List)// is nondet.
%
%   Match or generate a sequence of Element  where each pair of elements
%   is separated by Sep. When _parsing_,   a  matched Sep _commits_. The
%   final element is _not_ committed. See also sequence//5.

sequence(OnElem, OnSep, List) -->
    sequence_(List, OnElem, OnSep).

%!  sequence(:Start, :Element, :Sep, :End, ?List)// is semidet.
%
%   Match or generate a sequence of Element   enclosed by Start end End,
%   where each pair of elements is separated   by Sep. More formally, it
%   matches the following sequence:
%
%       Start (Element,Sep)*, End
%
%   The example below matches a Prolog list of integers:
%
%       ?- phrase(sequence(("[",blanks),
%                          number, (",",blanks),
%			   (blanks,"]"), L),
%			   `[1, 2, 3 ] a`, Tail).
%       L = [1, 2, 3],
%	Tail = [32, 97].

sequence(Start, OnElem, OnSep, End, List) -->
    Start,
    sequence_(List, OnElem, OnSep),
    End, !.

sequence_([H|T], P, Sep) -->
    call(P, H),
    (   {T == []}
    ->  []
    ;   Sep
    ->  !, sequence_(T, P, Sep)
    ;   {T = []}
    ).
sequence_([], _, _) -->
    [].


%!  optional(:Match, :Default)// is det.
%
%   Perform an optional  match,  executing  Default   if  Match  is  not
%   matched. This is comparable to ignore/1.  Both Match and Default are
%   DCG body terms. Default is typically  used to instantiate the output
%   variables of Match, but  may  also  be   used  to  match  a  default
%   representation.  Using  `[]`  for  Default    succeeds  without  any
%   additional actions if Match fails. For example:
%
%       ?- phrase(optional(number(X), {X=0}), `23`, Tail).
%       X = 23,
%       Tail = [].
%       ?- phrase(optional(number(X), {X=0}), `aap`, Tail).
%       X = 0,
%       Tail = `aap`.

optional(Match, _Default) -->
    Match, !.
optional(_, Default) -->
    Default, !.

%!  foreach(:Generator, :Element)// is det.
%!  foreach(:Generator, :Element, :Sep)// is det.
%
%   Generate a list from the  solutions   of  Generator.  This predicate
%   collects all solutions  of  Generator,   applies  Element  for  each
%   solution and Sep _between_ each pair of solutions.  For example:
%
%       ?- phrase(foreach(between(1,5,X), number(X), ", "), L).
%       L = "1, 2, 3, 4, 5, 6, 7, 8, 9, 10".

foreach(Generator, Rule) -->
    foreach(Generator, Rule, []).

foreach(Generator, Rule, Sep) -->
    { term_variables(Generator, GenVars0),   sort(GenVars0, GenVars),
      term_variables((Rule,Sep), GoalVars0), sort(GoalVars0, GoalVars),
      ord_subtract(GoalVars, GenVars, SharedGoalVars),
      ord_intersection(GenVars, GoalVars, SharedVars),
      Templ =.. [v|SharedVars],
      SharedTempl =.. [v|SharedGoalVars],
      findall(Templ, Generator, List)
    },
    emit_list(List, Templ, SharedTempl, Rule, Sep).

emit_list([], _, _, _, _) -->
    [].
emit_list([H|T], Templ, SharedTempl, OnElem, OnSep) -->
    { copy_term(t(Templ,SharedTempl,OnElem,OnSep),
                t(H,SharedTempl,OnElemCopy,OnSepCopy))
    },
    OnElemCopy,
    (   { T == [] }
    ->  []
    ;   OnSepCopy,
        emit_list(T, Templ, SharedTempl, OnElem, OnSep)
    ).


                /*******************************
                *            SANDBOX           *
                *******************************/

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(dcg_high_order:sequence/4).
sandbox:safe_meta_predicate(dcg_high_order:sequence/5).
sandbox:safe_meta_predicate(dcg_high_order:sequence/7).
sandbox:safe_meta_predicate(dcg_high_order:optional/4).
sandbox:safe_meta_predicate(dcg_high_order:foreach/4).
sandbox:safe_meta_predicate(dcg_high_order:foreach/5).
