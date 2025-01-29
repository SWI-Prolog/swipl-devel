/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
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

:- module(xsb_test_wfs,
          [ xsb_test_wfs/0
          ]).
:- use_module('../xsb_test').
:- use_module(library(plunit)).

xsb_test_wfs :-
    run_tests([ xsb_wfs
              ]).

:- meta_predicate
    swi_test(:).

term_expansion(wfs_test(Test),
               (   test(Test) :-
                       run_wfs_test(Test)
               )).

run_wfs_test(Test) :-
    xsb_test(wfs_tests,Test,xsb_test_wfs:swi_test(Test)).

swi_test(M:P) :-
    abolish_all_tables,
    M:query(P,Q,SGs,Tr,Un),
    call_top_query(P,M:Q),
    maplist(qualify(M), SGs, QSGs),
    maplist(qualify(M), Tr, QTr),
    maplist(qualify(M), Un, QUn),
    test_tv(QSGs,QTr,QUn).

call_top_query(P,Q) :-
	unqualify(Q, WQ),
	( numbervars(Q), write('Evaluating '), write(P), write(' : '),
          write_term(WQ, [priority(999), numbervars(true)]), nl, fail ; true ),
	( call(Q), fail ; true ).

test_tv([],_,_).
test_tv([SG|SGs], Trues, Undefs) :-
	find_tv(SG, T), real_tv(SG, Trues, Undefs, R), check_tv(SG, T, R),
	test_tv(SGs, Trues, Undefs).

find_tv(SG, _) :- SG, fail.
find_tv(SG, T) :- SG, !, ( tnot(SG) -> T = undefined ; T = true).
find_tv(_, false).

real_tv(SG, Trues, Undefs, R) :-
	( memberchk(SG, Trues) -> R = true
	; memberchk(SG, Undefs) -> R = undefined
	; R = false
	).

check_tv(QSG, T, R) :-
	unqualify(QSG, SG),
	write(SG), write(' is: '), write(T),
	( T == R -> writeln(' (OK)')
	; write(' should be: '), write(R), writeln(' (WRONG)')
	).

unqualify(_:Q, Q) :- !.
unqualify(Q, Q).

qualify(M, Q, M:Q).

:- begin_tests(xsb_wfs, [sto(rational_trees)]).

wfs_test(p06).
wfs_test(p07).
wfs_test(p08).
wfs_test(p09).
wfs_test(p10).
wfs_test(p11).
wfs_test(p12).
wfs_test(p13).
wfs_test(p14).
wfs_test(p15).
wfs_test(p16).
wfs_test(p17).
wfs_test(p18).
wfs_test(p19).
wfs_test(p20).
wfs_test(p21).
wfs_test(p22).
wfs_test(p23).
wfs_test(p24).
wfs_test(p25).
wfs_test(p26).
wfs_test(p27).
wfs_test(p29).
wfs_test(p30).
wfs_test(p31).
wfs_test(p32).
wfs_test(p33).
wfs_test(p34).
wfs_test(p35).
wfs_test(p36).
wfs_test(p37).
wfs_test(p39).
wfs_test(p40).
wfs_test(p42).
wfs_test(p43).
wfs_test(p44).
wfs_test(p45).
wfs_test(p46).
wfs_test(p47).
wfs_test(p48).
wfs_test(p49).
wfs_test(p50).
wfs_test(p51).
wfs_test(p52a).
wfs_test(p52).
wfs_test(p53).
wfs_test(p54).
wfs_test(p55).
wfs_test(p56).
wfs_test(p57).
wfs_test(p58).
wfs_test(p59).
wfs_test(p60).
wfs_test(p62).
wfs_test(p63).
wfs_test(p64).
wfs_test(p65).
wfs_test(p66).
wfs_test(p67).
wfs_test(p77).
wfs_test(p78).
wfs_test(p79).
wfs_test(p80).
wfs_test(p81).
wfs_test(p82).
wfs_test(p83).
wfs_test(p84).
wfs_test(p85).
wfs_test(p86).
wfs_test(p89).
wfs_test(p90).
wfs_test(p91).

:- end_tests(xsb_wfs).

