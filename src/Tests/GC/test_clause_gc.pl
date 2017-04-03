/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2010, University of Amsterdam
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

:- module(test_clause_gc,
	  [ test_clause_gc/0,
	    test_clause_gc/1
	  ]).

%%	test_clause_gc
%
%	This test runs a loop that builds  a list and for each iteration
%	runs clause/2 on a term that   contains  all relevant programing
%	constructs. The idea is to trigger   GC  and shifts at different
%	parts of this process.

test_clause_gc :-
	setup_call_cleanup(( prolog_stack_property(global, limit(GLimit)),
			     set_prolog_stack(global, limit(2*1024*1024)),
			     gspace(Cells),
			     MaxLen is max(20000, (Cells//2)//3)),
			   test_clause_gc(MaxLen),
			   set_prolog_stack(global, limit(GLimit))).

test_clause_gc(N) :-
	run(N, L),
	is_list(L).			% avoid L from being GC'ed

run(N, L) :-
	functor(H, cl, 8),
	clause(H, B),
	run(N, H, B, L).

run(N, H, B, [x|L]) :-
	succ(N2, N), !,
	functor(H2, cl, 8),
	clause(H2, B2),
	H =@= H2,
	B =@= B2,
	run(N2, H, B, L).
run(_, _, _, []).

:- '$clausable'(cl/8).

cl(a,
   A, A,
   1,
   1.2,
   1000000000000000000,
   100000000000000000000000000000000000000000000000000000,
   x(0,1,2,3,4,5,6,7)) :-
	a(a),
	a(B,B),
	a(1),
	a(1.2),
	a(1000000000000000000),
	a(100000000000000000000000000000000000000000000000000000),
	a(x(0,1,2,3,4,5,6,7)).

a(_).
a(_,_).

		 /*******************************
		 *	       UTIL		*
		 *******************************/

gspace(Cells) :-
        statistics(globallimit, Limit),
        statistics(globalused, Used),
	current_prolog_flag(address_bits, Wlen),
        Cells is (Limit-Used)//(Wlen//8).
