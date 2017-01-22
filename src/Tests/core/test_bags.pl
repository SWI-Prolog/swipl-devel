/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2014, University of Amsterdam
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

:- module(test_bags,
	  [ test_bags/0
	  ]).
:- use_module(library(plunit)).

test_bags :-
	run_tests([ bags
		  ]).

/** <module> Test findall, bagof, etc.

@tbd	Only tests new findnsols/4,5.  Other tests are in test.pl
*/

:- begin_tests(bags).

test(nsols, Lists == [ [1,2,3,4,5],
		       [6,7,8,9,10],
		       [11,12]
		     ]) :-
	findall(L, findnsols(5, I, between(1,12,I), L), Lists).
test(nsols_nested, Lists == [ [ [1,2,3,4,5],
				[6,7,8,9,10]
			      ],
			      [ [11,12]
			      ]
			    ]) :-
	findall(L1,
		findnsols(2, L,
			  findnsols(5, I, between(1,12,I), L),
			  L1),
		Lists).
test(nsols_commit, Lists == [ [ [1,2,3,4,5],
				[6,7,8,9,10]
			      ]
			    ]) :-
	findall(L1,
		( findnsols(2, L,
			    findnsols(5, I, between(1,12,I), L),
			    L1),
		  !
		),
		Lists).

:- end_tests(bags).
