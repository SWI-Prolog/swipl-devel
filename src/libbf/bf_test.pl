/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, University of Amsterdam
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

:- module(bf_test,
	  [ test/2,	% +Times,?Expr
	    bench/2,
	    log/1,
	    oracle_server/0,
	    test_bitwise_negation/1
	  ]).

/** <module> Test arithmetic by comparing to an oracle

This module provides a test framework   for  testing LibBF arithmetic by
creating random expressions and comparing their   result with the output
of another Prolog process.  To use it,

  - On one terminal run this using a trusted version of Prolog

	swipl bf_test.pl
        ?- oracle_server.

  - In another terminal run using the test version of Prolog

	swipl bf_test.pl
        ?- time(test(10 000, _)).

The `10 000` is the number of   random  expressions. The second argument
may be (partially) instantiated, e.g. `test(test(10  000, _*_))` to test
only multiplication.  See expression/1 down for the tested expressions.
*/

:- use_module(library(prolog_code)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(random)).

%!  test(+Times, ?Expr)

test(N, Expr) :-
    forall(between(1, N, I),
	   test1(I, Expr)).

test1(I, Expr) :-
    (   I mod 1 =:= 0
    ->  format(user_error, '\r~t~D~20|', [I])
    ;   true
    ),
    test(Expr).

test(Expr) :-
    expr(Expr),
    log_expression(Expr),
    catch(Us is Expr, UsError, true),
    catch(oracle(Expr, Them), ThemError, true),
    debug(expr, 'Expr: ~q --> ~q', [Expr, Us]),
    cmp_errors(UsError, ThemError, Expr),
    cmp_value(Us, Them, Expr).

cmp_errors(Us, Them, _), var(Us), var(Them) =>
    true.
cmp_errors(error(Formal, _), error(Formal, _), _) =>
    true.
cmp_errors(error(resource_error(stack), _), _, _) =>
    true.
cmp_errors(_, error(resource_error(stack), _), _) =>
    true.
cmp_errors(error(Us, _), error(Them, _), Expr) =>
    print_message(error, diff(Expr, error(Us, Them))).

cmp_value(Us, _, _), var(Us) =>
    true.
cmp_value(_, Them, _), var(Them) =>
    true.
cmp_value(Value, Value, _) =>
    true.
cmp_value(Us, Them, Expr) =>
    print_message(error, diff(Expr, value(Us, Them))).

bench(N, Expr) :-
    forall(between(1, N, _),
	   _ is Expr).

:- dynamic
   log_stream/1.

log(File) :-
    open(File, write, Out),
    asserta(log_stream(Out)).

log_expression(Expr), log_stream(Out) =>
    format(Out, '~k.~n', [Expr]),
    flush_output(Out).
log_expression(_) =>
    true.


%!  expression(?I, ?Expr)

term_expansion(expression(Expr), expression(I, Expr)) :-
    (   predicate_property(expression(_,_), number_of_clauses(I))
    ->  true
    ;   I = 0
    ).

expression(number+number).
expression(number-number).
expression(number*number).
expression(number/number).
expression(number**number).
expression(lsb(integer)).
expression(msb(integer)).
expression(popcount(integer)).
expression(gcd(integer,integer)).
%expression(integer<<integer).
%expression(integer>>integer).
expression(integer/\integer).
expression(integer\/integer).
%expression(\nonneg).

expr(VExp), ground(VExp) =>
    true.
expr(VExp), callable(VExp) =>
    most_general_goal(VExp, Templ),
    once(expression(_, Templ)),
    VExp =.. [Name|Args],
    Templ =.. [Name|TArgs],
    maplist(random, TArgs, Args).
expr(VExp), var(VExp) =>
    predicate_property(expression(_,_), number_of_clauses(Max)),
    I is random(Max),
    expression(I, Templ),
    most_general_goal(Templ, VExp),
    VExp =.. [Name|Args],
    Templ =.. [Name|TArgs],
    maplist(random, TArgs, Args).

random(_, N), number(N) =>
    true.
random(number, N) =>
    (   maybe(0.33)
    ->  random(float, N)
    ;   maybe(0.33)
    ->  random(integer, N)
    ;   random(rational, N)
    ).
random(integer, I) =>
    random(nonneg, I0),
    (   maybe
    ->  I = I0
    ;   I is -I0
    ).
random(nonneg, I) =>
    IMax is 1<<32,
    random_between(0, 100, Exp),
    random_between(0, IMax, Base),
    I is Base^Exp.
random(natural, I) =>
    random(nonneg, I0),
    I is I0 + 1.
random(bit, I) =>
    between(0, 10 000, I).
random(rational, R) =>
    random(integer, N),
    random(natural, D),
    R is N rdiv D.
random(float, X) =>
    random(pos_float, X0),
    (   maybe
    ->  X = X0
    ;   X is -X0
    ).
random(pos_float, X) =>
    random_between(-308,308,E),
    X is random_float * 10**E.

address(localhost:8042).

:- dynamic oracle_stream/1.

oracle_connect(Stream), oracle_stream(S) =>
    Stream = S.
oracle_connect(Stream) =>
    address(Address),
    tcp_connect(Address, S, []),
    asserta(oracle_stream(S)),
    Stream = S.

oracle(Expr, Value) :-
    oracle_connect(Stream),
    format(Stream, '~k.~n', [Expr]),
    flush_output(Stream),
    read(Stream, Reply),
    (   Reply = true(V)
    ->  Value = V
    ;   Reply = error(E)
    ->  throw(E)
    ).

oracle_server :-
    thread_create(oracle_server_, _, [alias(oracle)]).

oracle_server_ :-
    address(Address),
    tcp_socket(S),
    tcp_setopt(S, reuseaddr),
    tcp_bind(S, Address),
    tcp_listen(S, 5),
    accept_loop(S).

accept_loop(S) :-
    tcp_accept(S, Slave, _Peer),
    tcp_open_socket(Slave, Stream),
    thread_create(eval(Stream), _, [detached(true)]),
    accept_loop(S).

eval(Stream) :-
    read(Stream, Expr),
    catch(Result is Expr, E, true),
    (   var(E)
    ->  format(Stream, 'true(~k).~n', [Result])
    ;   E = error(Formal, _),
	format(Stream, 'error(error(~k, _)).~n', [Formal]),
	debug(error, 'Sending error ~q', [E])
    ),
    flush_output(Stream),
    eval(Stream).


		 /*******************************
		 *         SPECIFIC TESTS	*
		 *******************************/

%!  test_bitwise_negation(+IntExpr)
%
%   Test that \Expr create negated bits.

test_bitwise_negation(Expr) :-
    Num is Expr,
    Neg is \Num,
    Max is msb(abs(Num)),
    debug(bwneg, '~w --> ~w~n', [Num, Neg]),
    forall(between(0, Max, Bit),
	   cmp_bits_at(Bit, Num, Neg)).

cmp_bits_at(Bit, Num, Neg) :-
    bit(Bit, Num, NumBit),
    bit(Bit, Neg, NegBit),
    cmp_bits(Bit, NumBit, NegBit).

cmp_bits(_, 0, 1) => true.
cmp_bits(_, 1, 0) => true.
cmp_bits(I, A, B) => format('Bits at ~d: ~w - ~w~n', [I, A, B]), fail.

bit(At, Num, Bit) :-
    (   Num/\(1<<At) =:= 0
    ->  Bit = 0
    ;   Bit = 1
    ).
