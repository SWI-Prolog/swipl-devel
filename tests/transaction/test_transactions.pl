/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, University of Amsterdam
                         VU University Amsterdam
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

:- module(test_transactions,
          [ test_transactions/0,
            test_transaction/1,
            op(700, fx, ?)
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(ordsets)).

:- meta_predicate
    test_transaction(:).

/** <module> Test transaction

Test transaction handling.
*/

test_transactions :-
    run_tests([ transaction,
                thread_transaction
              ]).

:- begin_tests(transaction, [sto(rational_trees)]).

:- dynamic p/0, p/1.

cleanup :-
    retractall(p),
    retractall(p(_)).

test(assert, [cleanup(cleanup)]) :-
    test_transaction([tr([+p]), ?p]).
test(retract, [cleanup(cleanup)]) :-
    test_transaction([+p, tr([-p]), \+p]).
test(assert_retract, [cleanup(cleanup)]) :-
    test_transaction([tr([+p,-p]), \+p]).
test(nested1, [cleanup(cleanup)]) :-
    test_transaction([tr([+p(1), tr([+p(2)]), ?p(1),?p(2)])]).
test(nested2, [cleanup(cleanup)]) :-
    test_transaction([tr([+p(1), tr([+p(2),discard]), ?p(1),\+p(2)])]).
test(nested3, [cleanup(cleanup)]) :-
    test_transaction([tr([tr([])])]).
test(nested4, [cleanup(cleanup)]) :-
    test_transaction([tr([+p,tr([])]), ?p]).
test(nested5, [cleanup(cleanup)]) :-
    test_transaction([tr([a+p,tr([-p,\+p])])]).
test(nested6, [cleanup(cleanup)]) :-
    test_transaction([tr([a+p,tr([-p]),\+p])]).
test(nested7, [cleanup(cleanup)]) :-
    test_transaction([tr([z+p,tr([-p]),\+p])]).
test(nested8, [cleanup(cleanup)]) :-
    test_transaction([tr([+p,tr([-p,discard]),?p])]).
test(nested9, [cleanup(cleanup)]) :-
    assert(p),
    transaction((
        retract(p),
        transaction(\+p))).

test(update1, [cleanup(cleanup)]) :-
    test_transaction([tr([+p, u([z+p])])]).
test(update2, [cleanup(cleanup)]) :-
    test_transaction([tr([+p, +p(1), u([z+p,z+p(1)])])]).
test(update3, [cleanup(cleanup)]) :-
    test_transaction([+p, tr([-p, u([-p])])]).


:- end_tests(transaction).

:- begin_tests(thread_transaction,
               [ sto(rational_trees),
                 condition(current_prolog_flag(threads, true))
               ]).

:- dynamic p/0, p/1.

cleanup :-
    retractall(p),
    retractall(p(_)).

test(commit, [cleanup(cleanup)]) :-
    test_transaction([ in([+p,?p]),
                       ?p
                     ]).
test(discard, [cleanup(cleanup)]) :-
    test_transaction([ in([+p,?p,discard]),
                       \+p
                     ]).
test(discard2, [cleanup(cleanup)]) :-
    test_transaction([ in([+p,?p,discard,\+p]),
                       \+ p
                     ]).
test(isolate_assert, [cleanup(cleanup)]) :-
    test_transaction([ start(x),
                       in(x, +p),
                       \+p,
                       end(x),
                       ?p
                     ]).
test(isolate_retract, [cleanup(cleanup)]) :-
    test_transaction([ +p,
                       start(x),
                       in(x, -p),
                       ?p,
                       end(x),
                       \+p
                     ]).
test(isolate_retract2, [cleanup(cleanup)]) :-
    test_transaction([ +p,
                       start(x),
                       start(y),
                       in(x, -p),
                       in(y, -p),
                       ?p,
                       end(x),
                       \+p,
                       end(y),
                       \+p
                     ]).
test(isolate_retract2b, [cleanup(cleanup)]) :-
    test_transaction([ +p,
                       start(x),
                       start(y),
                       in(x, -p),
                       in(y, [?p]),     % y does not see x
                       ?p,
                       end(x),
                       \+p,
                       end(y)
                     ]).
test(isolate_retract2b, [cleanup(cleanup)]) :-
    test_transaction([ +p,
                       start(x),
                       -p,
                       \+p,
                       in(x, ?p),
                       end(x)
                     ]).

:- end_tests(thread_transaction).

%!  test_transaction(:List) is semidet.
%
%   Test transaction operations.  Operations in list are:
%
%     - asserta(Term)
%     - assertz(Term)
%       Assert a term.  assertz(Term) can be abbreviated as +Term.
%     - retractall(Term)
%     - retract(Term)
%       Acts as once(retract(Term)), Can be abbreviated as -(Term)
%     - start(Name)
%       Create a named thread running a transaction
%     - in(Name, Actions)
%       Run actions in the named thread
%     - end(Name)
%       End the named thread.
%     - in(Actions)
%       Run Actions in an anonymous thread in a transaction.  Combines
%       start(Name), in(Name, Actions), and end(Name).
%     - discard
%       Discard the current transaction and create a new one.
%     - u(List)
%       Verify the pending updates in a transaction.

test_transaction(M:List) :-
    must_be(list, List),
    anon_threads(Initial),
    test(List, 1, _{module:M}, State),
    cleanup(State),
    assertion(no_more_threads(Initial)).

test([], _, State, State).
test([discard|More], _, State, _) :-
    !,
    cleanup(State),
    throw(discarded(More)).
test([H|T], Step, State0, State) :-
    (   catch(action(H, State0, State1), Error, true)
    ->  (   var(Error)
        ->  Step1 is Step+1,
            test(T, Step1, State1, State)
        ;   print_message(error, Error)
        )
    ;   print_message(error, tr_failed(Step, H))
    ).

action(+(Term), State0, State) :-
    action(assertz(Term), State0, State).
action(a+(Term), State0, State) :- !,
    action(asserta(Term), State0, State).
action(z+(Term), State0, State) :- !,
    action(assertz(Term), State0, State).
action(-(Term), State0, State) :-
    action(retract(Term), State0, State).
action(?(Term), State0, State) :-
    action(expect(Term), State0, State).
action(\+(Term), State0, State) :-
    action(not_expect(Term), State0, State).

action(asserta(Term), State, State) :-
    M = State.module,
    asserta(M:Term).
action(assertz(Term), State, State) :-
    M = State.module,
    assertz(M:Term).
action(retract(Term), State, State) :-
    M = State.module,
    retract(M:Term),
    !.
action(expect(Term), State, State) :-
    M = State.module,
    call(M:Term),
    !.
action(not_expect(Term), State, State) :-
    M = State.module,
    \+ call(M:Term).
action(start(Name), State0, State) :-
    M = State0.module,
    thread_self(Main),
    thread_create(run(M, Main, []), Id, []),
    thread_get_message(started),
    State = State0.put(Name, Id).
action(end(Name), State0, State) :-
    del_dict(Name, State0, Thread, State),
    thread_send_message(Thread, done),
    thread_join(Thread).
action(in(Name, Actions), State, State) :-
    ensure_list(Actions, List),
    Thread = State.Name,
    thread_self(Main),
    thread_send_message(Thread, from(Main, List)),
    thread_get_message(Reply),
    assertion(Reply == true).
action(in(Actions), State, State) :-
    M = State.module,
    (   current_prolog_flag(threads, true)
    ->  thread_self(Main),
        thread_create(run(M, -, from(Main, Actions)), Thread, []),
        thread_get_message(Reply),
        assertion(Reply == true),
        thread_send_message(Thread, done),
        thread_join(Thread)
    ;   in_transaction(M, Actions)
    ).
action(tr(Actions), State, State) :-
    ensure_list(Actions, List),
    M = State.module,
    in_transaction(M, List).
action(u(List), State, State) :-
    transaction_updates(Clauses),
    M = State.module,
    maplist(update_action(M), Clauses, Terms),
    assertion(maplist(=@=, List, Terms)).

ensure_list(List, List) :-
    is_list(List),
    !.
ensure_list(Elem, [Elem]).

update_action(M, asserta(CRef), a+Term) :-
    clause_term(CRef, M, Term).
update_action(M, assertz(CRef), z+Term) :-
    clause_term(CRef, M, Term).
update_action(M, erased(CRef), -Term) :-
    clause_term(CRef, M, Term).

clause_term(CRef, M, Term) :-
    '$clause'(Head0, Body, CRef, _Bindings),
    (   Head0 = M:Head
    ->  true
    ;   Head = Head0
    ),
    (   Body == true
    ->  Term = Head
    ;   Term = (Head :- Body)
    ).

%!  in_transaction(+Module, +Actions)
%
%   Run Actions in a transaction.

in_transaction(Module, Actions) :-
    catch(transaction(tr_actions(Module, Actions)),
          discarded(Rest),
          in_transaction(Module, Rest)).

tr_actions(M, Actions) :-
    test(Actions, 1000, _{module:M}, State),
    cleanup(State).


%!  run(+Module, +MainThread, +Actions)
%
%   Run  Actions  inside  a  transaction  and    report  the  result  to
%   MainThread.

run(M, Main, Msg0) :-
    catch(transaction(actions(M, Main, Msg0)),
          discarded(Main2, Msg1),
          run(M, -, from(Main2, Msg1))).

actions(M, -, Msg0) :-
    !,
    actions(M, Msg0).
actions(M, Main, Msg0) :-
    thread_send_message(Main, started),
    actions(M, Msg0).

actions(M, Msg0) :-
    (   Msg0 == []
    ->  thread_get_message(Msg)
    ;   Msg = Msg0
    ),
    (   Msg == done
    ->  true
    ;   Msg = from(Main, Actions),
        (   catch(( must_be(list, Actions),
                    test(Actions, 1000, _{module:M}, State),
                    cleanup(State)
                  ),
                  Error, true)
        ->  (   var(Error)
            ->  thread_send_message(Main, true)
            ;   Error = discarded(Left)
            ->  throw(discarded(Main, Left))
            ;   thread_send_message(Main, error(Error))
            )
        ;   thread_send_message(Main, false)
        ),
        actions(M, [])
    ).

cleanup(State) :-
    dict_pairs(State, _, Pairs),
    maplist(clean_s, Pairs).

clean_s(module-_) :-
    !.
clean_s(discard-_) :-
    !.
clean_s(_Name-Thread) :-
    thread_send_message(Thread, done),
    thread_join(Thread).

anon_threads(Threads) :-
    current_prolog_flag(threads, true),
    !,
    findall(T, anon_thread(T), List),
    sort(List, Threads).
anon_threads([]).

no_more_threads(Initial) :-
    anon_threads(Now),
    ord_subtract(Now, Initial, []).

anon_thread(T) :-
    thread_property(T, id(_)),
    \+ thread_property(T, alias(_)).
