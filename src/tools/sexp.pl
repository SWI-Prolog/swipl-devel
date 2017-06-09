/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2013, University of Amsterdam
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

:- module(s_expression,
	  [ read_sexp_file/3		% +File, -ListOfSexp, +Options
	  ]).
:- use_module(library(record)).

:- record
	s_state(comment:boolean=false).

read_sexp_file(File, Terms, Options) :-
	make_s_state(Options, State, _),
	setup_call_cleanup(open(File, read, In),
			   (   get_code(In, C0),
			       phrase(sexps(C0, In, State), Terms)
			   ),
			   close(In)).

sexps(C0, In, State) -->
	sexp(C0, In, C, State),
	(   { C == -1 }
	->  []
	;   sexps(C, In, State)
	).

sexp(C0, In, C, State) -->
	ws(C0, In, C1, State),
	(   { C1 == -1 }
	->  { C = C1 }
	;   { C1 == 0'( }
	->  { get_code(In, C2),
	      phrase(list(C2, In, C, State), List)
	    },
	    [List]
	;   { end_of_atom(C1) }
	->  { char_code(Atom, C1),
	      get_code(In, C)
	    },
	    [Atom]
	;   { atom(C1, In, Atom, C, State) },
	    [Atom]
	).


%%	ws(+C0, +In, -C, +State)//
%
%	Skip white space and comment (; ... \n)

ws(-1, _, -1, _) --> !.
ws(C0, In, C, State) -->
	{ code_type(C0, space), !,
	  get_code(In, C1)
	},
	ws(C1, In, C, State).
ws(0';, In, C, State) --> !,
	(   { s_state_comment(State, true) }
	->  { get_code(In, C1),
	      skip_comment_leader(C1, In, C2),
	      read_upto(C2, 0'\n, In, Codes),
	      atom_codes(Comment, Codes)
	    },
	    [ comment(Comment) ]
	;   { skip(In, 0'\n)
	    }
	),
	{ get_code(In, C3) },
	ws(C3, In, C, State).
ws(C, _, C, _) --> [].

skip_comment_leader(-1, _, -1) :- !.
skip_comment_leader(WS, In, C) :-
	code_type(WS, space), !,
	get_code(In, C1),
	skip_comment_leader(C1, In, C).
skip_comment_leader(0';, In, C) :- !,
	get_code(In, C1),
	skip_comment_leader(C1, In, C).
skip_comment_leader(C, _, C).


list(C0, In, C, State) -->
	ws(C0, In, C1, State),
	(   { C1 == 0') }
	->  { get_code(In, C) }
	;   sexp(C1, In, C2, State),
	    list(C2, In, C, State)
	).

%%	atom(+C0, +In, -Atom, -C) is det.
%
%	Read an atom.  Special cases:
%
%		|...|		preserve case
%		"..."		Quoted
%		'...'		Quoted

atom(0'|, In, Atom, C, _) :- !,
	get_code(In, C1),
	read_upto(C1, 0'|, In, Codes),
	atom_codes(Atom, Codes),
	get_code(In, C).
atom(0'", In, Atom, C, _) :- !,
	get_code(In, C1),
	read_upto(C1, 0'", In, Codes),
	atom_codes(Atom, Codes),
	get_code(In, C).
atom(0'', In, Atom, C, _) :- !,
	get_code(In, C1),
	read_upto(C1, 0'', In, Codes),
	atom_codes(Atom, Codes),
	get_code(In, C).
atom(C0, In, Atom, C, _) :- !,
	read_non_ws(C0, In, Codes, C),
	name(Atom0, Codes),
	(   atom(Atom0)
	->  downcase_atom(Atom0, Atom)
	;   Atom = Atom0		% numbers
	).

read_upto(C, C, _, []) :- !.
read_upto(C, E, In, [C|T]) :-
	get_code(In, C1),
	read_upto(C1, E, In, T).

read_non_ws(C, _, [], C) :-
	end_of_atom(C), !.
read_non_ws(C0, In, [C0|T], C) :-
	get_code(In, C1),
	read_non_ws(C1, In, T, C).

end_of_atom(-1).
end_of_atom(0'().
end_of_atom(0')).
end_of_atom(C) :-
	code_type(C, space).

