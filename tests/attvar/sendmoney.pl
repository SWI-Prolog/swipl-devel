/*  Part of SWI-Prolog

    Author:        Tom Scrijvers
    E-mail:        tom.schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2011, K.U.Leuven
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

:- module(sendmoney,
	  [ sendmoney/0
	  ]).
:- use_module(library('clp/bounds')).

sendmoney :-
	send(X),
	X ==
	[    [9, 5, 6, 7],
	     [1, 0, 8, 5],
	  [1, 0, 6, 5, 2]
	].

send([[S,E,N,D],  [M,O,R,E],  [M,O,N,E,Y]])  :-
	Digits  = [S,E,N,D,M,O,R,Y],
	Carries = [C1,C2,C3,C4],
	Digits  in 0..9,
	Carries in 0..1,

	M                #=              C4,
	O  +  10  *  C4  #=  M  +  S  +  C3,
	N  +  10  *  C3  #=  O  +  E  +  C2,
	E  +  10  *  C2  #=  R  +  N  +  C1,
	Y  +  10  *  C1  #=  E  +  D,

	M  #>=  1,
	S  #>=  1,
	all_different(Digits),
	label(Digits).
