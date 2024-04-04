/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, SWI-Prolog Solutions b.v.
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

% Test data for qlf round trip of various integer formats.  Limits
% and critical instructions:
%
% Critical integer ranges:
%
%   - 32-bit H_SMALLINT: -2147483648 .. 2147483647
%   - Tagged integers:   -72057594037927936 .. 72057594037927935
%
% Instructions:
%
%   - H_SMALLINT, B_SMALLINT, A_INTEGER
%     Tagged integers in head, body or arithmetic (-O)
%   - H_SMALLINTW, B_SMALLINTW, A_INTEGERW
%     32-bit machines tagged integers outside -2147483648 .. 2147483647
%   - A_ADD_FC
%     Simple addition.  On 32-bit systems limited to -2147483648 .. 2147483647
%     Otherwise to tagged integers
%   - B_UNIFY_FC, B_UNIFY_VC, B_EQ_VC, B_NEQ_VC
%     Portable: tagged version must fit 32 bits: 25 bit integer:
%     -16777216 .. 16777215
%
% Portability:
%
%   Prolog flag `portable_vmi` must generate VM code on 64 bit platforms
%   that can be loaded on 32 bit platforms.   This may imply relocating.

% Test H_SMALLINT, H_SMALLINTW and H_MPZ
% Unification in the head
head(0).
head(-1).
head(1).
head(-2147483648).
head(2147483647).
head(-2147483649).
head(2147483648).
head(-72057594037927936).
head(72057594037927935).
head(-72057594037927937).
head(72057594037927936).
head(-476462786578645564756252).
head(476462786578645564756252).

% test B_SMALLINT, B_SMALLINTW and B_MPZ
% Push integer for subgoal
body(X) :- echo(0, X).
body(X) :- echo(-1, X).
body(X) :- echo(1, X).
body(X) :- echo(-2147483648, X).
body(X) :- echo(2147483647, X).
body(X) :- echo(-2147483649, X).
body(X) :- echo(2147483648, X).
body(X) :- echo(-72057594037927936, X).
body(X) :- echo(72057594037927935, X).
body(X) :- echo(-72057594037927937, X).
body(X) :- echo(72057594037927936, X).
body(X) :- echo(-476462786578645564756252, X).
body(X) :- echo(476462786578645564756252, X).

echo(X,X).

% Test A_INTEGER, A_INTEGERW and A_MPZ
% Push integer to arithmetic evaluation stack
% :- set_prolog_flag(optimise, true).
expr(X) :- X is 0+0.
expr(X) :- X is -1+0.
expr(X) :- X is 1+0.
expr(X) :- X is -2147483648+0.
expr(X) :- X is 2147483647+0.
expr(X) :- X is -2147483649+0.
expr(X) :- X is 2147483648+0.
expr(X) :- X is -72057594037927936+0.
expr(X) :- X is 72057594037927935+0.
expr(X) :- X is -72057594037927937+0.
expr(X) :- X is 72057594037927936+0.
expr(X) :- X is -476462786578645564756252+0.
expr(X) :- X is 476462786578645564756252+0.

% Test A_ADD_FC.  Note that the first argument must be a "first var"
% for this optimization to be applied.
add(X) :- p(A), Y is A+1, X = Y.
add(X) :- p(A), Y is A-1, X = Y.
add(X) :- p(A), Y is A+2147483647, X = Y.
add(X) :- p(A), Y is A+ -2147483648, X = Y.
add(X) :- p(A), Y is A+2147483648, X = Y.   % general arithmetic on 32 bit
add(X) :- p(A), Y is A+ -2147483649, X = Y. % general arithmetic on 32 bit

p(0).

cmp(X) :- echo(a,A), (A == a -> X = 0 ; X = 1).
cmp(X) :- echo(0,A), (A == 0 -> X = 0 ; X = 1).
cmp(X) :- echo(0,A), (A == 1 -> X = 1 ; X = 0).
cmp(X) :- echo(-1,A), (A == -1 -> X = 0 ; X = 1).
cmp(X) :- echo(16777215,A), (A == 16777215 -> X = 0 ; X = 1).
cmp(X) :- echo(16777216,A), (A == 16777216 -> X = 0 ; X = 1).
cmp(X) :- echo(-16777216,A), (A == -16777216 -> X = 0 ; X = 1).
cmp(X) :- echo(-16777216,A), (A == 16777216 -> X = 1 ; X = 0).
cmp(X) :- echo(-16777217,A), (A == -16777217 -> X = 0 ; X = 1).


% Test rational number instructions.  These have
% a uniform representation, so we do not need to
% test many alternatives.
rat(23673r276348).
rat(-23673r276348).
rat(36433737686427865845854r347489547594758975684).
rat(X) :- echo(-36r42, X).
rat(X) :- X is 237r373 * 253r236.

test(head(_)).
test(body(_)).
test(expr(_)).
test(add(_)).
test(cmp(_)).
test(rat(_)).

run :-
    forall((test(G), G), format('~q.~n', [G])).
