/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2017, VU University Amsterdam
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

:- module(solution_sequences,
          [ distinct/1,                 % :Goal
            distinct/2,                 % ?Witness, :Goal
            reduced/1,                  % :Goal
            reduced/3,                  % ?Witness, :Goal, +Options
            limit/2,                    % +Limit, :Goal
            offset/2,                   % +Offset, :Goal
            call_nth/2,                 % :Goal, ?Nth
            order_by/2,                 % +Spec, :Goal
            group_by/4                  % +By, +Template, :Goal, -Bag
          ]).
:- use_module(library(nb_set)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(option)).

/** <module> Modify solution sequences

The meta predicates of this library modify  the sequence of solutions of
a goal. The modifications and  the  predicate   names  are  based on the
classical database operations DISTINCT,  LIMIT,   OFFSET,  ORDER  BY and
GROUP BY.

These   predicates   were   introduced   in     the   context   of   the
[SWISH](http://swish.swi-prolog.org) Prolog browser-based   shell, which
can represent the solutions to a predicate  as a table. Notably wrapping
a goal in distinct/1 avoids duplicates  in   the  result table and using
order_by/2 produces a nicely ordered table.

However, the predicates from this  library  can   also  be  used to stay
longer within the clean paradigm  where non-deterministic predicates are
composed  from  simpler  non-deterministic  predicates    by   means  of
conjunction and disjunction. While evaluating   a  conjunction, we might
want to eliminate duplicates of the first part of the conjunction. Below
we give both the classical  solution   for  solving variations of (a(X),
b(X)) and the ones using this library side-by-side.

  $ Avoid duplicates of earlier steps :

    ==
      setof(X, a(X), Xs),               distinct(a(X)),
      member(X, Xs),                    b(X)
      b(X).
    ==

    Note that the distinct/1 based solution returns the first result
    of distinct(a(X)) immediately after a/1 produces a result, while
    the setof/3 based solution will first compute all results of a/1.

  $ Only try b(X) only for the top-10 a(X) :

    ==
      setof(X, a(X), Xs),               limit(10, order_by([desc(X)], a(X))),
      reverse(Xs, Desc),                b(X)
      first_max_n(10, Desc, Limit),
      member(X, Limit),
      b(X)
    ==

    Here we see power of composing primitives from this library and
    staying within the paradigm of pure non-deterministic relational
    predicates.

@see all solution predicates findall/3, bagof/3 and setof/3.
@see library(aggregate)
*/

:- meta_predicate
    distinct(0),
    distinct(?, 0),
    reduced(0),
    reduced(?, 0, +),
    limit(+, 0),
    offset(+, 0),
    call_nth(0, ?),
    order_by(+, 0),
    group_by(?, ?, 0, -).

:- noprofile((
       distinct/1,
       distinct/2,
       reduced/1,
       reduced/2,
       limit/2,
       offset/2,
       call_nth/2,
       order_by/2,
       group_by/3)).


%!  distinct(:Goal).
%!  distinct(?Witness, :Goal).
%
%   True if Goal is true and  no   previous  solution  of Goal bound
%   Witness to the same  value.  As   previous  answers  need  to be
%   copied, equivalence testing is based on _term variance_ (=@=/2).
%   The variant distinct/1 is equivalent to distinct(Goal,Goal).
%
%   If the answers are ground terms,   the  predicate behaves as the
%   code below, but answers are  returned   as  soon  as they become
%   available rather than first computing the complete answer set.
%
%     ==
%     distinct(Goal) :-
%         findall(Goal, Goal, List),
%         list_to_set(List, Set),
%         member(Goal, Set).
%     ==

distinct(Goal) :-
    distinct(Goal, Goal).
distinct(Witness, Goal) :-
    term_variables(Witness, Vars),
    Witness1 =.. [v|Vars],
    empty_nb_set(Set),
    call(Goal),
    add_nb_set(Witness1, Set, true).

%!  reduced(:Goal).
%!  reduced(?Witness, :Goal, +Options).
%
%   Similar to distinct/1, but does  not   guarantee  unique  results in
%   return for using a limited  amount   of  memory. Both distinct/1 and
%   reduced/1  create  a  table  that    block  duplicate  results.  For
%   distinct/1,  this  table  may  get  arbitrary  large.  In  contrast,
%   reduced/1 discards the table and starts a  new one of the table size
%   exceeds a specified limit. This filter   is  useful for reducing the
%   number of answers when  processing  large   or  infinite  long  tail
%   distributions. Options:
%
%     - size_limit(+Integer)
%     Max number of elements kept in the table.  Default is 10,000.

reduced(Goal) :-
    reduced(Goal, Goal, []).
reduced(Witness, Goal, Options) :-
    option(size_limit(SizeLimit), Options, 10_000),
    term_variables(Witness, Vars),
    Witness1 =.. [v|Vars],
    empty_nb_set(Set),
    State = state(Set),
    call(Goal),
    reduced_(State, Witness1, SizeLimit).

reduced_(State, Witness1, SizeLimit) :-
    arg(1, State, Set),
    add_nb_set(Witness1, Set, true),
    size_nb_set(Set, Size),
    (   Size > SizeLimit
    ->  empty_nb_set(New),
        nb_setarg(1, State, New)
    ;   true
    ).


%!  limit(+Count, :Goal)
%
%   Limit the number of solutions. True   if Goal is true, returning
%   at most Count solutions. Solutions are  returned as soon as they
%   become  available.

limit(Count, Goal) :-
    Count > 0,
    State = count(0),
    call(Goal),
    arg(1, State, N0),
    N is N0+1,
    (   N =:= Count
    ->  !
    ;   nb_setarg(1, State, N)
    ).

%!  offset(+Count, :Goal)
%
%   Ignore the first Count  solutions.  True   if  Goal  is true and
%   produces more than Count solutions.  This predicate computes and
%   ignores the first Count solutions.

offset(Count, Goal) :-
    Count > 0,
    !,
    State = count(0),
    call(Goal),
    arg(1, State, N0),
    (   N0 >= Count
    ->  true
    ;   N is N0+1,
        nb_setarg(1, State, N),
        fail
    ).
offset(Count, Goal) :-
    Count =:= 0,
    !,
    call(Goal).
offset(Count, _) :-
    domain_error(not_less_than_zero, Count).

%!  call_nth(:Goal, ?Nth)
%
%   True when Goal succeeded for the Nth time. If Nth is bound on entry,
%   the predicate succeeds deterministically if there   are at least Nth
%   solutions for Goal.

call_nth(Goal, Nth) :-
    integer(Nth),
    !,
    (   Nth > 0
    ->  (   call_nth(Goal, Sofar),
            Sofar =:= Nth
        ->  true
        )
    ;   domain_error(not_less_than_one, Nth)
    ).
call_nth(Goal, Nth) :-
    var(Nth),
    !,
    State = count(0),
    call(Goal),
    arg(1, State, N0),
    Nth is N0+1,
    nb_setarg(1, State, Nth).
call_nth(_Goal, Bad) :-
    must_be(integer, Bad).

%!  order_by(+Spec, :Goal)
%
%   Order solutions according to Spec.  Spec   is  a  list of terms,
%   where each element is one of. The  ordering of solutions of Goal
%   that only differ in variables that are _not_ shared with Spec is
%   not changed.
%
%     - asc(Term)
%     Order solution according to ascending Term
%     - desc(Term)
%     Order solution according to descending Term

order_by(Spec, Goal) :-
    must_be(list, Spec),
    non_empty_list(Spec),
    maplist(order_witness, Spec, Witnesses0),
    join_orders(Witnesses0, Witnesses),
    non_witness_template(Goal, Witnesses, Others),
    reverse(Witnesses, RevWitnesses),
    maplist(x_vars, RevWitnesses, WitnessVars),
    Template =.. [v,Others|WitnessVars],
    findall(Template, Goal, Results),
    order(RevWitnesses, 2, Results, OrderedResults),
    member(Template, OrderedResults).

order([], _, Results, Results).
order([H|T], N, Results0, Results) :-
    order1(H, N, Results0, Results1),
    N2 is N + 1,
    order(T, N2, Results1, Results).

order1(asc(_), N, Results0, Results) :-
    sort(N, @=<, Results0, Results).
order1(desc(_), N, Results0, Results) :-
    sort(N, @>=, Results0, Results).

non_empty_list([]) :-
    !,
    domain_error(non_empty_list, []).
non_empty_list(_).

order_witness(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
order_witness(asc(Term), asc(Witness)) :-
    !,
    witness(Term, Witness).
order_witness(desc(Term), desc(Witness)) :-
    !,
    witness(Term, Witness).
order_witness(Term, _) :-
    domain_error(order_specifier, Term).

x_vars(asc(Vars), Vars).
x_vars(desc(Vars), Vars).

witness(Term, Witness) :-
    term_variables(Term, Vars),
    Witness =.. [v|Vars].

%!  join_orders(+SpecIn, -SpecOut) is det.
%
%   Merge  subsequent  asc  and   desc    sequences.   For  example,
%   [asc(v(A)), asc(v(B))] becomes [asc(v(A,B))].

join_orders([], []).
join_orders([asc(O1)|T0], [asc(O)|T]) :-
    !,
    ascs(T0, OL, T1),
    join_witnesses(O1, OL, O),
    join_orders(T1, T).
join_orders([desc(O1)|T0], [desc(O)|T]) :-
    !,
    descs(T0, OL, T1),
    join_witnesses(O1, OL, O),
    join_orders(T1, T).

ascs([asc(A)|T0], [A|AL], T) :-
    !,
    ascs(T0, AL, T).
ascs(L, [], L).

descs([desc(A)|T0], [A|AL], T) :-
    !,
    descs(T0, AL, T).
descs(L, [], L).

join_witnesses(O, [], O) :- !.
join_witnesses(O, OL, R) :-
    term_variables([O|OL], VL),
    R =.. [v|VL].

%!  non_witness_template(+Goal, +Witness, -Template) is det.
%
%   Create a template for the bindings  that   are  not  part of the
%   witness variables.

non_witness_template(Goal, Witness, Template) :-
    ordered_term_variables(Goal, AllVars),
    ordered_term_variables(Witness, WitnessVars),
    ord_subtract(AllVars, WitnessVars, TemplateVars),
    Template =.. [t|TemplateVars].

ordered_term_variables(Term, Vars) :-
    term_variables(Term, Vars0),
    sort(Vars0, Vars).

%!  group_by(+By, +Template, :Goal, -Bag) is nondet.
%
%   Group bindings of Template that have the same value for By. This
%   predicate  is  almost  the  same  as  bagof/3,  but  instead  of
%   specifying  the  existential  variables  we   specify  the  free
%   variables. It is provided for  consistency and complete coverage
%   of the common database vocabulary.

group_by(By, Template, Goal, Bag) :-
    ordered_term_variables(Goal, GVars),
    ordered_term_variables(By+Template, UVars),
    ord_subtract(GVars, UVars, ExVars),
    bagof(Template, ExVars^Goal, Bag).
