/*  Part of SWI-Prolog

    Author:        Markus Triska and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2005-2011, University of Amsterdam
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

:- module(linprog, [linprog/0]).

:- dynamic user:file_search_path/2.
:- prolog_load_context(directory, Dir),
   atom_concat(Dir, '/../../../packages/clpqr', Lib0),
   absolute_file_name(Lib0, Lib),
   asserta(user:file_search_path(library, Lib)).

:- if((current_prolog_flag(bounded, false),	% GMP implies rational
       exists_source(library(clpr)))).		% package not available

:- use_module(library('clp/simplex')).

linprog :-
	radiation,
	transport.


radiation :-
	gen_state(S0),
	constraint([0.3*x1, 0.1*x2] =< 2.7, S0, S1),
	constraint([0.5*x1, 0.5*x2] = 6, S1, S2),
	constraint([0.6*x1, 0.4*x2] >= 6, S2, S3),
	constraint([x1] >= 0, S3, S4),
	constraint([x2] >= 0, S4, S5),
	minimize([0.4*x1, 0.5*x2], S5, S),
	Obj is 21 rdiv 4,
	X1 is 15 rdiv 2,
	X2 is 9 rdiv 2,
	objective(S, Obj),
	variable_value(S, x1, X1),
	variable_value(S, x2, X2).

transport :-
	transportation([12, 7, 14], [3, 15, 9, 6],
		[[20, 50, 10, 60],
		 [70, 40, 60, 30],
		 [40, 80, 70, 40]], Matrix),
	Matrix == [[0, 3, 9, 0],
		   [0, 7, 0, 0],
		   [3, 5, 0, 6]].

:- else.

linprog.

:- endif.
