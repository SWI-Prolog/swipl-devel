/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2014, University of Amsterdam,
                              VU University Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Copyright notes: findall/3, bagof/3 and setof/3 are part of the standard
folklore of Prolog. The core  is  findall/3   based  on  C code that was
written for SWI-Prolog. Older versions also used C-based implementations
of  bagof/3  and  setof/3.  As   these    proved   wrong,   the  current
implementation is modelled  after  an  older   version  of  Yap.  Ulrich
Neumerkel fixed the variable preservation of   bagof/3 and setof/3 using
an algorithm also found in  Yap  6.3,   where  it  is claimed: "uses the
SICStus algorithm to guarantee that variables will have the same names".
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module('$bags',
          [ findall/3,                  % +Templ, :Goal, -List
            findall/4,                  % +Templ, :Goal, -List, +Tail
            findnsols/4,                % +Count, +Templ, :Goal, -List
            findnsols/5,                % +Count, +Templ, :Goal, -List, +Tail
            bagof/3,                    % +Templ, :Goal, -List
            setof/3                     % +Templ, :Goal, -List
          ]).

:- meta_predicate
    findall(?, 0, -),
    findall(?, 0, -, ?),
    findnsols(+, ?, 0, -),
    findnsols(+, ?, 0, -, ?),
    bagof(?, ^, -),
    setof(?, ^, -).

:- noprofile((
        findall/4,
        findall/3,
        findnsols/4,
        findnsols/5,
        bagof/3,
        setof/3,
        findall_loop/4)).

:- '$iso'((findall/3,
           bagof/3,
           setof/3)).

%!  findall(-Var, +Goal, -Bag) is det.
%!  findall(-Var, +Goal, -Bag, +Tail) is det.
%
%   Bag holds all alternatives for Var  in  Goal.   Bag  might  hold
%   duplicates.   Equivalent  to bagof, using the existence operator
%   (^) on all free variables of Goal.  Succeeds with Bag  =  []  if
%   Goal fails immediately.
%
%   The  findall/4  variation  is  a    difference-list  version  of
%   findall/3.

findall(Templ, Goal, List) :-
    findall(Templ, Goal, List, []).

findall(Templ, Goal, List, Tail) :-
    setup_call_cleanup(
        '$new_findall_bag',
        findall_loop(Templ, Goal, List, Tail),
        '$destroy_findall_bag').

findall_loop(Templ, Goal, List, Tail) :-
    (   Goal,
        '$add_findall_bag'(Templ)   % fails
    ;   '$collect_findall_bag'(List, Tail)
    ).

%!  findnsols(+Count, @Template, :Goal, -List) is nondet.
%!  findnsols(+Count, @Template, :Goal, -List, ?Tail) is nondet.
%
%   True when List is the next chunk of maximal Count instantiations
%   of Template that reprensents a solution of Goal.  For example:
%
%     ==
%     ?- findnsols(5, I, between(1, 12, I), L).
%     L = [1, 2, 3, 4, 5] ;
%     L = [6, 7, 8, 9, 10] ;
%     L = [11, 12].
%     ==
%
%   @compat Ciao, but the SWI-Prolog version is non-deterministic.
%   @error  domain_error(not_less_than_zero, Count) if Count is less
%           than 0.
%   @error  type_error(integer, Count) if Count is not an integer.

findnsols(Count, Template, Goal, List) :-
    findnsols(Count, Template, Goal, List, []).

findnsols(Count, Template, Goal, List, Tail) :-
    integer(Count),
    !,
    findnsols2(count(Count), Template, Goal, List, Tail).
findnsols(Count, Template, Goal, List, Tail) :-
    Count = count(Integer),
    integer(Integer),
    !,
    findnsols2(Count, Template, Goal, List, Tail).
findnsols(Count, _, _, _, _) :-
    '$type_error'(integer, Count).

findnsols2(Count, Template, Goal, List, Tail) :-
    nsols_count(Count, N), N > 0,
    !,
    copy_term(Template+Goal, Templ+G),
    setup_call_cleanup(
        '$new_findall_bag',
        findnsols_loop(Count, Templ, G, List, Tail),
        '$destroy_findall_bag').
findnsols2(Count, _, _, List, Tail) :-
    nsols_count(Count, 0),
    !,
    Tail = List.
findnsols2(Count, _, _, _, _) :-
    nsols_count(Count, N),
    '$domain_error'(not_less_than_zero, N).

findnsols_loop(Count, Templ, Goal, List, Tail) :-
    nsols_count(Count, FirstStop),
    State = state(FirstStop),
    (   call_cleanup(Goal, Det=true),
        '$add_findall_bag'(Templ, Found),
        Det \== true,
        arg(1, State, Found),
        '$collect_findall_bag'(List, Tail),
        (   '$suspend_findall_bag'
        ;   nsols_count(Count, Incr),
            NextStop is Found+Incr,
            nb_setarg(1, State, NextStop),
            fail
        )
    ;   '$collect_findall_bag'(List, Tail)
    ).

nsols_count(count(N), N).

%!  bagof(+Var, +Goal, -Bag) is semidet.
%
%   Implements Clocksin and  Melish's  bagof/3   predicate.  Bag  is
%   unified with the alternatives of Var  in Goal, Free variables of
%   Goal are bound,  unless  asked  not   to  with  the  existential
%   quantifier operator (^).

bagof(Templ, Goal0, List) :-
    '$free_variable_set'(Templ^Goal0, Goal, Vars),
    (   Vars == v
    ->  findall(Templ, Goal, List),
        List \== []
    ;   findall(Vars-Templ, Goal, Answers),
        bind_bagof_keys(Answers,_),
        keysort(Answers, Sorted),
        pick(Sorted, Vars, List)
    ).

%!  bind_bagof_keys(+VarsTemplPairs, -SharedVars)
%
%   Establish a canonical binding  of   the  _vars_ structures. This
%   code   was   added    by    Ulrich     Neumerkel    in    commit
%   1bf9e87900b3bbd61308e80a784224c856854745.

bind_bagof_keys([], _).
bind_bagof_keys([W-_|WTs], Vars) :-
    term_variables(W, Vars, _),
    bind_bagof_keys(WTs, Vars).

pick(Bags, Vars1, Bag1) :-
    pick_first(Bags, Vars0, Bag0, RestBags),
    select_bag(RestBags, Vars0, Bag0, Vars1, Bag1).

select_bag([], Vars0, Bag0, Vars1, Bag1) :-   % last one: deterministic
    !,
    Vars0 = Vars1,
    Bag0 = Bag1.
select_bag(_, Vars, Bag, Vars, Bag).
select_bag(RestBags, _, _, Vars1, Bag1) :-
    pick(RestBags, Vars1, Bag1).

%!  pick_first(+Bags, +Vars, -Bag1, -RestBags) is semidet.
%
%   Pick the first result-bag from the   list  of Templ-Answer. Note
%   that we pick all elements that are  equal under =@=, but because
%   the variables in the witness are canonized this is the same as ==.
%
%   @param Bags     List of Templ-Answer
%   @param Vars     Initial Templ (for rebinding variables)
%   @param Bag1     First bag of results
%   @param RestBags Remaining Templ-Answer

pick_first([Vars-Templ|T0], Vars, [Templ|T], RestBag) :-
    pick_same(T0, Vars, T, RestBag).


pick_same([V-H|T0], Vars, [H|T], Bag) :-
    V == Vars,
    !,
    pick_same(T0, Vars, T, Bag).
pick_same(Bag, _, [], Bag).


%!  setof(+Var, +Goal, -Set) is semidet.
%
%   Equivalent to bagof/3, but sorts the   resulting bag and removes
%   duplicate answers. We sort  immediately   after  the  findall/3,
%   removing duplicate Templ-Answer pairs early.

setof(Templ, Goal0, List) :-
    '$free_variable_set'(Templ^Goal0, Goal, Vars),
    (   Vars == v
    ->  findall(Templ, Goal, Answers),
        Answers \== [],
        sort(Answers, List)
    ;   findall(Vars-Templ, Goal, Answers),
        (   ground(Answers)
        ->  sort(Answers,Sorted),
            pick(Sorted,Vars,List)
        ;   bind_bagof_keys(Answers,_VDict),
            sort(Answers, Sorted),
            pick(Sorted, Vars, Listu),
            sort(Listu,List) % Listu ordering may be nixed by Vars
        )
    ).
