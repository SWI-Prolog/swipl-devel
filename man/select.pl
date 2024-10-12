/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2013-2024, VU University Amsterdam
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

:- use_module(library(dcg/basics)).
:- use_module(library(debug)).
:- use_module(library(readutil)).
:- use_module(library(main)).

:- initialization(main, main).

main([Def,File]) :-
	setup_call_cleanup(
	    open(File, read, In),
	    (	read_line_to_codes(In, Line),
		process(Line, In, Def, [])
	    ),
	    close(In)).

process(end_of_file, _, _, State) :- !,
	assertion(State == []).
process(Line, In, DefA, State) :-
	phrase(("#ifdef", whites, string(Def), whites), Line), !,
	(   atom_codes(DefA, Def)
	->  Cond = true
	;   Cond = false
	),
	read_line_to_codes(In, Line2),
	process(Line2, In, DefA, [Cond|State]).
process(Line, In, DefA, State) :-
	phrase(("#ifndef", whites, string(Def), whites), Line), !,
	(   atom_codes(DefA, Def)
	->  Cond = false
	;   Cond = true
	),
	read_line_to_codes(In, Line2),
	process(Line2, In, DefA, [Cond|State]).
process(Line, In, Def, State) :-
	phrase(("#else", whites), Line), !,
	read_line_to_codes(In, Line2),
	State = [Old|State2],
	(   Old == true
	->  New = false
	;   New = true
	),
	process(Line2, In, Def, [New|State2]).
process(Line, In, Def, State) :-
	phrase(("#endif", whites), Line), !,
	read_line_to_codes(In, Line2),
	State = [_|State2],
	process(Line2, In, Def, State2).
process(_Line, In, Def, State) :-
	State = [false|_], !,
	read_line_to_codes(In, Line2),
	process(Line2, In, Def, State).
process(Line, In, Def, State) :-
	format('~s~n', [Line]),
	read_line_to_codes(In, Line2),
	process(Line2, In, Def, State).

