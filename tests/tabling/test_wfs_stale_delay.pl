/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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


:- module(test_wfs_stale_delay,
          [ test_wfs_stale_delay/0
          ]).
:- use_module(library(plunit)).

/** <module> Simplification must not loop on a removed delay set

When propagation removes a  delay set from  an answer, the elements of
that set stay  in the answer's delay buffer.  answer_delays_on() used to
count them, so the answer stayed registered as depending on a table it no
longer depends on.  If that table ends up without answers,
simplify_component() propagated to the answer in every pass without
changing anything, and never terminated.

The program below was found by testing random programs and reduced.  Its
well-founded model is {a, f}; evaluating f first used to hang.
*/

test_wfs_stale_delay :-
    run_tests([ wfs_stale_delay
              ]).

:- table a/0, b/0, c/0, d/0, e/0, f/0.

a :- f.
b :- c.
c :- d, c.
d :- e.
e :- tnot(a), tnot(b).
e :- d.
f :- tnot(c).

truth(G, T) :-
    (   call_delays(G, Delays)
    ->  (   Delays == true
        ->  T = true
        ;   T = undefined
        )
    ;   T = false
    ).

:- begin_tests(wfs_stale_delay).

test(model, Model == [a=true, b=false, c=false, d=false, e=false, f=true]) :-
    abolish_all_tables,
    f,                                  % this order used to loop
    findall(G=T,
            ( member(G, [a,b,c,d,e,f]),
              truth(G, T)
            ), Model).

:- end_tests(wfs_stale_delay).
