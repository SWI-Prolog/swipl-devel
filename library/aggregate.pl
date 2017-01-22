/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2016, University of Amsterdam
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

:- module(aggregate,
          [ foreach/2,                  % :Generator, :Goal
            aggregate/3,                % +Templ, :Goal, -Result
            aggregate/4,                % +Templ, +Discrim, :Goal, -Result
            aggregate_all/3,            % +Templ, :Goal, -Result
            aggregate_all/4,            % +Templ, +Discrim, :Goal, -Result
            free_variables/4            % :Generator, :Template, +Vars0, -Vars
          ]).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- meta_predicate
    foreach(0,0),
    aggregate(?,^,-),
    aggregate(?,?,^,-),
    aggregate_all(?,0,-),
    aggregate_all(?,?,0,-).

/** <module> Aggregation operators on backtrackable predicates

This library provides aggregating operators  over   the  solutions  of a
predicate. The operations are a generalisation   of the bagof/3, setof/3
and findall/3 built-in predicates. The   defined  aggregation operations
are counting, computing the sum, minimum,   maximum,  a bag of solutions
and a set of solutions. We first   give  a simple example, computing the
country with the smallest area:

==
smallest_country(Name, Area) :-
        aggregate(min(A, N), country(N, A), min(Area, Name)).
==

There are four aggregation predicates (aggregate/3, aggregate/4, aggregate_all/3 and aggregate/4), distinguished on two properties.

    $ aggregate vs. aggregate_all :
    The aggregate predicates use setof/3 (aggregate/4) or bagof/3
    (aggregate/3), dealing with existential qualified variables
    (Var^Goal) and providing multiple solutions for the remaining free
    variables in Goal. The aggregate_all/3 predicate uses findall/3,
    implicitly qualifying all free variables and providing exactly one
    solution, while aggregate_all/4 uses sort/2 over solutions that
    Discriminator (see below) generated using findall/3.

    $ The Discriminator argument :
    The versions with 4 arguments deduplicate redundant solutions of
    Goal. Solutions for which both the template variables and
    Discriminator are identical will be treated as one solution. For
    example, if we wish to compute the total population of all
    countries, and for some reason =|country(belgium, 11000000)|= may
    succeed twice, we can use the following to avoid counting the
    population of Belgium twice:

    ==
        aggregate(sum(P), Name, country(Name, P), Total)
    ==

All aggregation predicates support  the   following  operators  below in
Template. In addition, they allow for  an arbitrary named compound term,
where each of the arguments is a term  from the list below. For example,
the term r(min(X), max(X)) computes both the minimum and maximum binding
for X.

        * count
        Count number of solutions.  Same as sum(1).
        * sum(Expr)
        Sum of Expr for all solutions.
        * min(Expr)
        Minimum of Expr for all solutions.
        * min(Expr, Witness)
        A term min(Min, Witness), where Min is the minimal version
        of Expr over all solutions, and Witness is any other template
        applied to solutions that produced Min.  If multiple
        solutions provide the same minimum, Witness corresponds to
        the first solution.
        * max(Expr)
        Maximum of Expr for all solutions.
        * max(Expr, Witness)
        As min(Expr, Witness), but producing the maximum result.
        * set(X)
        An ordered set with all solutions for X.
        * bag(X)
        A list of all solutions for X.

*Acknowledgements*

_|The development of this library was sponsored by SecuritEase,
  http://www.securitease.com
|_

@compat Quintus, SICStus 4. The forall/2 is a SWI-Prolog built-in and
        term_variables/3 is a SWI-Prolog built-in with
        *|different semantics|*.
@tbd    Analysing the aggregation template and compiling a predicate
        for the list aggregation can be done at compile time.
@tbd    aggregate_all/3 can be rewritten to run in constant space using
        non-backtrackable assignment on a term.
*/

                 /*******************************
                 *           AGGREGATE          *
                 *******************************/

%!  aggregate(+Template, :Goal, -Result) is nondet.
%
%   Aggregate bindings in Goal according to Template.  The aggregate/3
%   version performs bagof/3 on Goal.

aggregate(Template, Goal0, Result) :-
    template_to_pattern(bag, Template, Pattern, Goal0, Goal, Aggregate),
    bagof(Pattern, Goal, List),
    aggregate_list(Aggregate, List, Result).

%!  aggregate(+Template, +Discriminator, :Goal, -Result) is nondet.
%
%   Aggregate bindings in Goal according to Template.  The aggregate/4
%   version performs setof/3 on Goal.

aggregate(Template, Discriminator, Goal0, Result) :-
    template_to_pattern(bag, Template, Pattern, Goal0, Goal, Aggregate),
    setof(Discriminator-Pattern, Goal, Pairs),
    pairs_values(Pairs, List),
    aggregate_list(Aggregate, List, Result).

%!  aggregate_all(+Template, :Goal, -Result) is semidet.
%
%   Aggregate  bindings  in  Goal   according    to   Template.  The
%   aggregate_all/3 version performs findall/3 on   Goal.  Note that
%   this predicate fails if Template contains one or more of min(X),
%   max(X),  min(X,Witness)  or  max(X,Witness)  and   Goal  has  no
%   solutions, i.e., the minumum and  maximum   of  an  empty set is
%   undefined.

aggregate_all(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
aggregate_all(count, Goal, Count) :-
    !,
    aggregate_all(sum(1), Goal, Count).
aggregate_all(sum(X), Goal, Sum) :-
    !,
    State = state(0),
    (  call(Goal),
           arg(1, State, S0),
           S is S0 + X,
           nb_setarg(1, State, S),
           fail
    ;  arg(1, State, Sum)
    ).
aggregate_all(max(X), Goal, Max) :-
    !,
    State = state(X),
    (  call(Goal),
           arg(1, State, M0),
           M is max(M0,X),
           nb_setarg(1, State, M),
           fail
    ;  arg(1, State, Max),
           nonvar(Max)
    ).
aggregate_all(min(X), Goal, Min) :-
    !,
    State = state(X),
    (  call(Goal),
           arg(1, State, M0),
           M is min(M0,X),
           nb_setarg(1, State, M),
           fail
    ;  arg(1, State, Min),
           nonvar(Min)
    ).
aggregate_all(max(X,W), Goal, max(Max,Witness)) :-
    !,
    State = state(false, _Max, _Witness),
    (  call(Goal),
           (   State = state(true, Max0, _)
           ->  X > Max0,
               nb_setarg(2, State, X),
               nb_setarg(3, State, W)
           ;   number(X)
           ->  nb_setarg(1, State, true),
               nb_setarg(2, State, X),
               nb_setarg(3, State, W)
           ;   type_error(number, X)
           ),
           fail
    ;  State = state(true, Max, Witness)
    ).
aggregate_all(min(X,W), Goal, min(Min,Witness)) :-
    !,
    State = state(false, _Min, _Witness),
    (  call(Goal),
           (   State = state(true, Min0, _)
           ->  X < Min0,
               nb_setarg(2, State, X),
               nb_setarg(3, State, W)
           ;   number(X)
           ->  nb_setarg(1, State, true),
               nb_setarg(2, State, X),
               nb_setarg(3, State, W)
           ;   type_error(number, X)
           ),
           fail
    ;  State = state(true, Min, Witness)
    ).
aggregate_all(Template, Goal0, Result) :-
    template_to_pattern(all, Template, Pattern, Goal0, Goal, Aggregate),
    findall(Pattern, Goal, List),
    aggregate_list(Aggregate, List, Result).

%!  aggregate_all(+Template, +Discriminator, :Goal, -Result) is semidet.
%
%   Aggregate  bindings  in  Goal   according    to   Template.  The
%   aggregate_all/4 version performs findall/3 followed by sort/2 on
%   Goal. See aggregate_all/3 to understand   why this predicate can
%   fail.

aggregate_all(Template, Discriminator, Goal0, Result) :-
    template_to_pattern(all, Template, Pattern, Goal0, Goal, Aggregate),
    findall(Discriminator-Pattern, Goal, Pairs0),
    sort(Pairs0, Pairs),
    pairs_values(Pairs, List),
    aggregate_list(Aggregate, List, Result).

template_to_pattern(All, Template, Pattern, Goal0, Goal, Aggregate) :-
    template_to_pattern(Template, Pattern, Post, Vars, Aggregate),
    existential_vars(Goal0, Goal1, AllVars, Vars),
    clean_body((Goal1, Post), Goal2),
    (   All == bag
    ->  add_existential_vars(AllVars, Goal2, Goal)
    ;   Goal = Goal2
    ).

existential_vars(Var, Var) -->
    { var(Var) },
    !.
existential_vars(Var^G0, G) -->
    !,
    [Var],
    existential_vars(G0, G).
existential_vars(M:G0, M:G) -->
    !,
    existential_vars(G0, G).
existential_vars(G, G) -->
    [].

add_existential_vars([], G, G).
add_existential_vars([H|T], G0, H^G1) :-
    add_existential_vars(T, G0, G1).


%!  clean_body(+Goal0, -Goal) is det.
%
%   Remove redundant =true= from Goal0.

clean_body((Goal0,Goal1), Goal) :-
    !,
    clean_body(Goal0, GoalA),
    clean_body(Goal1, GoalB),
    (   GoalA == true
    ->  Goal = GoalB
    ;   GoalB == true
    ->  Goal = GoalA
    ;   Goal = (GoalA,GoalB)
    ).
clean_body(Goal, Goal).


%!  template_to_pattern(+Template, -Pattern, -Post, -Vars, -Aggregate)
%
%   Determine which parts of the goal we must remember in the
%   findall/3 pattern.
%
%   @param Post is a body-term that evaluates expressions to reduce
%               storage requirements.
%   @param Vars is a list of intermediate variables that must be
%               added to the existential variables for bagof/3.
%   @param Aggregate defines the aggregation operation to execute.

template_to_pattern(Term, Pattern, Goal, Vars, Aggregate) :-
    templ_to_pattern(Term, Pattern, Goal, Vars, Aggregate),
    !.
template_to_pattern(Term, Pattern, Goal, Vars, term(MinNeeded, Functor, AggregateArgs)) :-
    compound(Term),
    !,
    Term =.. [Functor|Args0],
    templates_to_patterns(Args0, Args, Goal, Vars, AggregateArgs),
    needs_one(AggregateArgs, MinNeeded),
    Pattern =.. [Functor|Args].
template_to_pattern(Term, _, _, _, _) :-
    invalid_template(Term).

templ_to_pattern(sum(X),           X,         true,    [],   sum) :- var(X), !.
templ_to_pattern(sum(X0),          X,         X is X0, [X0], sum) :- !.
templ_to_pattern(count,            1,         true,    [],   count) :- !.
templ_to_pattern(min(X),           X,         true,    [],   min) :- var(X), !.
templ_to_pattern(min(X0),          X,         X is X0, [X0], min) :- !.
templ_to_pattern(min(X0, Witness), X-Witness, X is X0, [X0], min_witness) :- !.
templ_to_pattern(max(X0),          X,         X is X0, [X0], max) :- !.
templ_to_pattern(max(X0, Witness), X-Witness, X is X0, [X0], max_witness) :- !.
templ_to_pattern(set(X),           X,         true,    [],   set) :- !.
templ_to_pattern(bag(X),           X,         true,    [],   bag) :- !.

templates_to_patterns([], [], true, [], []).
templates_to_patterns([H0], [H], G, Vars, [A]) :-
    !,
    sub_template_to_pattern(H0, H, G, Vars, A).
templates_to_patterns([H0|T0], [H|T], (G0,G), Vars, [A0|A]) :-
    sub_template_to_pattern(H0, H, G0, V0, A0),
    append(V0, RV, Vars),
    templates_to_patterns(T0, T, G, RV, A).

sub_template_to_pattern(Term, Pattern, Goal, Vars, Aggregate) :-
    templ_to_pattern(Term, Pattern, Goal, Vars, Aggregate),
    !.
sub_template_to_pattern(Term, _, _, _, _) :-
    invalid_template(Term).

invalid_template(Term) :-
    callable(Term),
    !,
    domain_error(aggregate_template, Term).
invalid_template(Term) :-
    type_error(aggregate_template, Term).

%!  needs_one(+Ops, -OneOrZero)
%
%   If one of the operations in Ops needs at least one answer,
%   unify OneOrZero to 1.  Else 0.

needs_one(Ops, 1) :-
    member(Op, Ops),
    needs_one(Op),
    !.
needs_one(_, 0).

needs_one(min).
needs_one(min_witness).
needs_one(max).
needs_one(max_witness).

%!  aggregate_list(+Op, +List, -Answer) is semidet.
%
%   Aggregate the answer  from  the   list  produced  by  findall/3,
%   bagof/3 or setof/3. The latter  two   cases  deal  with compound
%   answers.
%
%   @tbd    Compile code for incremental state update, which we will use
%           for aggregate_all/3 as well.  We should be using goal_expansion
%           to generate these clauses.

aggregate_list(bag, List0, List) :-
    !,
    List = List0.
aggregate_list(set, List, Set) :-
    !,
    sort(List, Set).
aggregate_list(sum, List, Sum) :-
    sum_list(List, Sum).
aggregate_list(count, List, Count) :-
    length(List, Count).
aggregate_list(max, List, Sum) :-
    max_list(List, Sum).
aggregate_list(max_witness, List, max(Max, Witness)) :-
    max_pair(List, Max, Witness).
aggregate_list(min, List, Sum) :-
    min_list(List, Sum).
aggregate_list(min_witness, List, min(Min, Witness)) :-
    min_pair(List, Min, Witness).
aggregate_list(term(0, Functor, Ops), List, Result) :-
    !,
    maplist(state0, Ops, StateArgs, FinishArgs),
    State0 =.. [Functor|StateArgs],
    aggregate_term_list(List, Ops, State0, Result0),
    finish_result(Ops, FinishArgs, Result0, Result).
aggregate_list(term(1, Functor, Ops), [H|List], Result) :-
    H =.. [Functor|Args],
    maplist(state1, Ops, Args, StateArgs, FinishArgs),
    State0 =.. [Functor|StateArgs],
    aggregate_term_list(List, Ops, State0, Result0),
    finish_result(Ops, FinishArgs, Result0, Result).

aggregate_term_list([], _, State, State).
aggregate_term_list([H|T], Ops, State0, State) :-
    step_term(Ops, H, State0, State1),
    aggregate_term_list(T, Ops, State1, State).


%!  min_pair(+Pairs, -Key, -Value) is det.
%!  max_pair(+Pairs, -Key, -Value) is det.
%
%   True if Key-Value has the  smallest/largest   key  in  Pairs. If
%   multiple pairs share the smallest/largest key, the first pair is
%   returned.

min_pair([M0-W0|T], M, W) :-
    min_pair(T, M0, W0, M, W).

min_pair([], M, W, M, W).
min_pair([M0-W0|T], M1, W1, M, W) :-
    (   M0 < M1
    ->  min_pair(T, M0, W0, M, W)
    ;   min_pair(T, M1, W1, M, W)
    ).

max_pair([M0-W0|T], M, W) :-
    max_pair(T, M0, W0, M, W).

max_pair([], M, W, M, W).
max_pair([M0-W0|T], M1, W1, M, W) :-
    (   M0 > M1
    ->  max_pair(T, M0, W0, M, W)
    ;   max_pair(T, M1, W1, M, W)
    ).

%!  step(+AggregateAction, +New, +State0, -State1).

step(bag,   X, [X|L], L).
step(set,   X, [X|L], L).
step(count, _, X0, X1) :-
    succ(X0, X1).
step(sum,   X, X0, X1) :-
    X1 is X0+X.
step(max,   X, X0, X1) :-
    X1 is max(X0, X).
step(min,   X, X0, X1) :-
    X1 is min(X0, X).
step(max_witness, X-W, X0-W0, X1-W1) :-
    (   X > X0
    ->  X1 = X, W1 = W
    ;   X1 = X0, W1 = W0
    ).
step(min_witness, X-W, X0-W0, X1-W1) :-
    (   X < X0
    ->  X1 = X, W1 = W
    ;   X1 = X0, W1 = W0
    ).
step(term(Ops), Row, Row0, Row1) :-
    step_term(Ops, Row, Row0, Row1).

step_term(Ops, Row, Row0, Row1) :-
    functor(Row, Name, Arity),
    functor(Row1, Name, Arity),
    step_list(Ops, 1, Row, Row0, Row1).

step_list([], _, _, _, _).
step_list([Op|OpT], Arg, Row, Row0, Row1) :-
    arg(Arg, Row, X),
    arg(Arg, Row0, X0),
    arg(Arg, Row1, X1),
    step(Op, X, X0, X1),
    succ(Arg, Arg1),
    step_list(OpT, Arg1, Row, Row0, Row1).

finish_result(Ops, Finish, R0, R) :-
    functor(R0, Functor, Arity),
    functor(R, Functor, Arity),
    finish_result(Ops, Finish, 1, R0, R).

finish_result([], _, _, _, _).
finish_result([Op|OpT], [F|FT], I, R0, R) :-
    arg(I, R0, A0),
    arg(I, R, A),
    finish_result1(Op, F, A0, A),
    succ(I, I2),
    finish_result(OpT, FT, I2, R0, R).

finish_result1(bag, Bag0, [], Bag) :-
    !,
    Bag = Bag0.
finish_result1(set, Bag,  [], Set) :-
    !,
    sort(Bag, Set).
finish_result1(max_witness, _, M-W, R) :-
    !,
    R = max(M,W).
finish_result1(min_witness, _, M-W, R) :-
    !,
    R = min(M,W).
finish_result1(_, _, A, A).

%!  state0(+Op, -State, -Finish)

state0(bag,   L, L).
state0(set,   L, L).
state0(count, 0, _).
state0(sum,   0, _).

%!  state1(+Op, +First, -State, -Finish)

state1(bag, X, L, [X|L]) :- !.
state1(set, X, L, [X|L]) :- !.
state1(_,   X, X, _).


                 /*******************************
                 *             FOREACH          *
                 *******************************/

%!  foreach(:Generator, :Goal)
%
%   True if conjunction of results is   true. Unlike forall/2, which
%   runs a failure-driven loop that proves Goal for each solution of
%   Generator, foreach/2 creates a conjunction.   Each member of the
%   conjunction is a copy of  Goal,   where  the variables it shares
%   with Generator are filled with the values from the corresponding
%   solution.
%
%   The implementation executes forall/2 if   Goal  does not contain
%   any variables that are not shared with Generator.
%
%   Here is an example:
%
%   ==
%   ?- foreach(between(1,4,X), dif(X,Y)), Y = 5.
%   Y = 5.
%   ?- foreach(between(1,4,X), dif(X,Y)), Y = 3.
%   false.
%   ==
%
%   @bug    Goal is copied repeatedly, which may cause problems if
%           attributed variables are involved.

foreach(Generator, Goal) :-
    term_variables(Generator, GenVars0), sort(GenVars0, GenVars),
    term_variables(Goal, GoalVars0), sort(GoalVars0, GoalVars),
    ord_subtract(GoalVars, GenVars, SharedGoalVars),
    (   SharedGoalVars == []
    ->  \+ (Generator, \+Goal)      % = forall(Generator, Goal)
    ;   ord_intersection(GenVars, GoalVars, SharedVars),
        Templ =.. [v|SharedVars],
        SharedTempl =.. [v|SharedGoalVars],
        findall(Templ, Generator, List),
        prove_list(List, Templ, SharedTempl, Goal)
    ).

prove_list([], _, _, _).
prove_list([H|T], Templ, SharedTempl, Goal) :-
    copy_term(Templ+SharedTempl+Goal,
              H+SharedTempl+Copy),
    Copy,
    prove_list(T, Templ, SharedTempl, Goal).


%!  free_variables(:Generator, +Template, +VarList0, -VarList) is det.
%
%   Find free variables in bagof/setof template.  In order to handle
%   variables  properly,  we  have  to   find  all  the  universally
%   quantified variables in the  Generator.   All  variables  as yet
%   unbound are universally quantified, unless
%
%       1. they occur in the template
%       2. they are bound by X^P, setof/3, or bagof/3
%
%   free_variables(Generator, Template, OldList, NewList) finds this
%   set using OldList as an accumulator.
%
%   @author Richard O'Keefe
%   @author Jan Wielemaker (made some SWI-Prolog enhancements)
%   @license Public domain (from DEC10 library).
%   @tbd Distinguish between control-structures and data terms.
%   @tbd Exploit our built-in term_variables/2 at some places?

free_variables(Term, Bound, VarList, [Term|VarList]) :-
    var(Term),
    term_is_free_of(Bound, Term),
    list_is_free_of(VarList, Term),
    !.
free_variables(Term, _Bound, VarList, VarList) :-
    var(Term),
    !.
free_variables(Term, Bound, OldList, NewList) :-
    explicit_binding(Term, Bound, NewTerm, NewBound),
    !,
    free_variables(NewTerm, NewBound, OldList, NewList).
free_variables(Term, Bound, OldList, NewList) :-
    functor(Term, _, N),
    free_variables(N, Term, Bound, OldList, NewList).

free_variables(0, _, _, VarList, VarList) :- !.
free_variables(N, Term, Bound, OldList, NewList) :-
    arg(N, Term, Argument),
    free_variables(Argument, Bound, OldList, MidList),
    M is N-1,
    !,
    free_variables(M, Term, Bound, MidList, NewList).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

explicit_binding(\+ _Goal,             Bound, fail,     Bound      ) :- !.
explicit_binding(not(_Goal),           Bound, fail,     Bound      ) :- !.
explicit_binding(Var^Goal,             Bound, Goal,     Bound+Var) :- !.
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.

%!  term_is_free_of(+Term, +Var) is semidet.
%
%   True if Var does not appear  in   Term.  This has been rewritten
%   from the DEC10 library source   to exploit our non-deterministic
%   arg/3.

term_is_free_of(Term, Var) :-
    \+ var_in_term(Term, Var).

var_in_term(Term, Var) :-
    Var == Term,
    !.
var_in_term(Term, Var) :-
    compound(Term),
    arg(_, Term, Arg),
    var_in_term(Arg, Var),
    !.


%!  list_is_free_of(+List, +Var) is semidet.
%
%   True if Var is not in List.

list_is_free_of([Head|Tail], Var) :-
    Head \== Var,
    !,
    list_is_free_of(Tail, Var).
list_is_free_of([], _).


%       term_variables(+Term, +Vars0, -Vars) is det.
%
%       True if Vars is the union of variables in Term and Vars0.
%       We cannot have this as term_variables/3 is already defined
%       as a difference-list version of term_variables/2.

%term_variables(Term, Vars0, Vars) :-
%       term_variables(Term+Vars0, Vars).


%!  sandbox:safe_meta(+Goal, -Called) is semidet.
%
%   Declare the aggregate meta-calls safe. This cannot be proven due
%   to the manipulations of the argument Goal.

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(aggregate:foreach/2).
sandbox:safe_meta_predicate(aggregate:aggregate/3).
sandbox:safe_meta_predicate(aggregate:aggregate/4).
sandbox:safe_meta_predicate(aggregate:aggregate_all/3).
sandbox:safe_meta_predicate(aggregate:aggregate_all/4).

