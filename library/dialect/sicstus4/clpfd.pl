/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(sicstus4_clpfd,
	  [ domain/3,
	    (table)/3,
	    op(760, yfx, #<=>),
	    op(750, xfy, #=>),
	    op(750, yfx, #<=),
	    op(700, xfx, in_set)
	  ]).
:- reexport('../../clp/clpfd').
:- reexport('../../clp/clpfd',
	    [ (#<==>)/2 as (#<=>),
	      (#==>)/2 as (#=>),
	      (#<==)/2 as (#<=),
	      fd_inf/2 as fd_min,
	      fd_sup/2 as fd_max,
	      tuples_in/2 as (table)
	    ]).

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(clpfd, sicstus4_clpfd).

/** <module> SICStus 4 library(clpfd)

@tbd	This library is incomplete and is missing various features
	supported by SICStus library(clpfd).
@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dclpfd.html
*/

:- op(760, yfx, user:(#<=>)).
:- op(750, xfy, user:(#=>)).
:- op(750, yfx, user:(#<=)).
:- op(700, xfx, user:(in_set)).

domain(Vars, Min, Max) :- Vars ins Min..Max.

%!	table(+Tuples, +Extension, +Options).
%
%	@tbd	No options are supported yet. Fails if Options is not
%		empty.

table(Tuples, Extension, []) :-
	table(Tuples, Extension).
