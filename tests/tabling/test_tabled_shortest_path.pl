/*  Part of SWI-Prolog

    Author:        Oskar Haarklou Veileborg & Jan Wielemaker
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2024, Oskar Haarklou Veileborg
                         SWI-Prolog Solutions b.v.
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

:- module(test_tabled_shortest_path,
          [ test_tabled_shortest_path/0
          ]).
:- use_module(library(plunit)).

% Test created from #1222

test_tabled_shortest_path :-
    run_tests([ tabled_shortest_path
              ]).

:- begin_tests(tabled_shortest_path,
               [ setup(abolish_all_tables),
                 cleanup(abolish_all_tables)
               ]).

test(path, Path == 14-[26-16,16-14,14-11]) :-
    path(26, 11, Path).

:- table path(_,_,min).

path(A, A, Ans) :- Ans = 0-[].
path(A, B, Ans) :-
    road(A, C, T), path(C, B, R), R = PrevD-Tail, Dist is PrevD + T,
    Ans = Dist-[A-C|Tail].

road(0,2,3). road(0,8,5). road(0,12,5). road(0,23,4). road(1,6,1). road(2,8,1). road(3,8,1).
road(3,23,5). road(3,27,5). road(4,8,1). road(4,13,2). road(4,15,9). road(4,26,4). road(5,25,4).
road(5,27,6). road(6,10,5). road(6,16,7). road(6,23,6). road(7,1,4). road(7,12,3). road(8,0,1).
road(8,12,6). road(8,16,3). road(8,19,4). road(8,23,5). road(8,24,8). road(9,6,4). road(9,7,3).
road(9,11,9). road(9,15,3). road(9,17,3). road(9,18,5). road(9,27,4). road(10,2,7). road(10,8,5).
road(10,16,1). road(10,20,3). road(11,14,3). road(11,19,3). road(12,0,5). road(12,2,4).
road(12,5,6). road(12,19,8). road(12,24,4). road(13,12,4). road(14,1,1). road(14,10,3).
road(14,11,9). road(15,1,6). road(15,4,6). road(15,6,5). road(15,12,2). road(15,14,5).
road(15,21,5). road(16,10,7). road(16,14,4). road(17,3,2). road(17,20,1). road(17,21,8).
road(18,17,2). road(19,1,5). road(19,17,2). road(20,10,1). road(20,13,4). road(20,17,9).
road(20,27,4). road(21,3,4). road(21,5,6). road(21,10,3). road(21,28,2). road(22,2,6).
road(22,9,4). road(22,20,2). road(23,2,3). road(23,4,8). road(23,5,2). road(23,12,3).
road(23,22,5). road(23,29,1). road(24,0,4). road(24,16,2). road(25,8,2). road(25,11,3).
road(25,21,5). road(25,22,1). road(25,24,4). road(26,16,1). road(26,29,2). road(27,2,7).
road(27,6,5). road(27,18,1). road(27,29,2). road(28,5,6). road(28,12,7). road(28,22,6).
road(28,25,5). road(29,0,5). road(29,4,2). road(29,6,5). road(29,20,6).

:- end_tests(tabled_shortest_path).
