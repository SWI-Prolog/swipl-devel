/*  File:    sexp.pl
    Author:  Jan Wielemaker
    Created: Sep  2 2005
    Purpose:
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

