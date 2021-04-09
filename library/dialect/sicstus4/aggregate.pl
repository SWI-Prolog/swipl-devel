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

:- module(sicstus4_aggregate,
	  [ term_variables/3,		% +Term, +Vars0, -Vars
	    % This predicate is built-in on SWI.
	    % We re-export it here to avoid warnings when SICStus code
	    % explicitly imports it from library(aggregate).
	    forall/2			% :Generator, :Goal
	  ]).
:- reexport('../../aggregate').

:- multifile sicstus4:rename_module/2.

sicstus4:rename_module(aggregate, sicstus4_aggregate).

/** <module> SICStus 4 library(aggregate).

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus/lib_002daggregate.html
*/

%!	term_variables(+Term, +Vars0, -Vars) is det.
%
%	Vars is the union of Vars0 and all variables that occur in Term.
%	This is not the same as SWI-Prolog's built-in term_variables/3 -
%	the argument order is different and this predicate is guaranteed
%	to return a list without duplicates.

term_variables(Term, Vars0, Vars) :-
	system:term_variables(Term, TermVars, Vars0),
	sort(TermVars, Vars).
