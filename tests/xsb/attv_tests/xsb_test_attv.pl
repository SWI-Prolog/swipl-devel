/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019-2025, VU University Amsterdam
		              CWI, Amsterdam
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

:- module(xsb_test_attv,
          [ xsb_test_attv/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_attv :-
    run_tests(xsb_attv_tests).

:- begin_tests(xsb_attv_tests, [sto(rational_trees)]).

% Disabled tests
%
%  - assert_attv
%  - attvar_assert_regalloc
%    We do not support attributed variables in assert/retract
%  - interrupt2
%  - pre_image
%    put_attributes/2 and get_attributes/2 seem to set/get a single
%    term representing the attributes.  When put_attr/3 and get_attr/3
%    are used, this is a term `[Mod1,Value1,Mod2,Value2|...]`. Otherwise
%    it can be an arbitrary term.  We do not support that.  This test
%    relies on verify_attributes/2, which we also do not support.
%  - tabled_attv
%    Also broken in current XSB.  Needs to be investigated.
%  - trie_assert_attv
%    Relies in assert of attributed terms and trie indexes.
%  - trie_assert_attv2
%    Relies verify_attributes/2 and the same as trie_assert_attv
%  - trie_intern_attv
%    Replies on interning.

goal_expansion(xsb_test(Test, Goal),
               xsb_test(attv_tests, Test, Goal)).

%test(assert_attv) :- xsb_test(assert_attv, test).
%test(attvar_assert_regalloc) :- xsb_test(attvar_assert_regalloc, test).
test(attv_test) :- xsb_test(attv_test, test).
test(copyterm_attv) :- xsb_test(copyterm_attv, test).
test(fd1) :- xsb_test(fd1, test).
test(findall_attv) :- xsb_test(findall_attv, test).
test(general) :- xsb_test(general, test).
test(interrupt1) :- xsb_test(interrupt1, test).
%test(interrupt2) :- xsb_test(interrupt2, test).
%test(pre_image) :- xsb_test(pre_image, test).
test(ret_attv) :- xsb_test(ret_attv, test).
%test(tabled_attv) :- xsb_test(tabled_attv, test).
%test(trie_assert_attv) :- xsb_test(trie_assert_attv, test).
%test(trie_assert_attv2) :- xsb_test(trie_assert_attv2, test).
%test(trie_intern_attv) :- xsb_test(trie_intern_attv, test).


:- end_tests(xsb_attv_tests).
