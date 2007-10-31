/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(regex_compat,
	  [ regex_emacs_to_advanced/2,
	    regex_convert_file/1,
	    regex_main/0
	  ]).
:- use_module(library(pce)).
:- use_module(library(lists)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file deals with translating  old-style   (<  6.5.12)  EMACS regular
expressions into new style ones. Typically, a file is converted using

	?- regex_convert_file(+File)
	
Creating a file <File>.new after  the   conversion.  Check the file, for
example using mgdiff or another (visual)   diff  tool before overwriting
the old file! 

Differences
===========

Translation of Emacs regex to new advanced regex:

	\(..\|..\)	-->	(..|..)
	(		-->	\(
	|		-->	\|
	)		-->	\)
	{		-->	\{
	}		-->	\}
	\s<space>	-->	\s
	\S<space>	-->	\S
	\sd		-->	\d
	\Sd		-->	\D
	\sn		-->	\n		(newline)
	\su		-->	[[:upper:]]
	\sl		-->	[[:lower:]]
	\s.		-->	[[:punct:]]
	\S.		-->	[^[:punct:]]
	[\]		-->	[\\]		(Bracket expressions honour \)

About this module
=================

This module is  a  rather  crude   implementation  for  an  approach  of
converting Prolog source code using the Prolog parser. It loads the file
into an XPCE text_buffer, reads the  file using position information and
records the change-requests. In a second stage   it makes the changes to
the text_buffer and saves the result.

Notably adapting the current  syntax   (operators  etc.)  is incomplete,
which can cause this module to raise an exception.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	regex_main
%	
%	By adding the  following  line  to   the  start  of  this  file,
%	adjusting the path, you turn this file into a PrologScript (Unix
%	only).
%	
%	#!/usr/bin/pl -q -g regex_main -s

regex_main :-
	current_prolog_flag(argv, Argv),
	append(_, [--|Files], Argv),
	maplist(regex_convert_file, Files),
	halt.

%%	regex_convert_file(+File)
%	
%	Convert a single file, creating  <File>.new   on  success. It is
%	adviced the inspect the changes before   moving  the .new to the
%	original file.

regex_convert_file(File) :-
	format(user_error, 'Converting ~w ...', [File]),
	new(V, view),
	get(V, text_buffer, TB),
	send(TB, insert_file, 0, File),
	pce_open(TB, read, In),
	read_term(In, Term, [subterm_positions(Pos)]),
	phrase(convert_loop(Term, Pos, In, File), Substitutions),
	close(In),
	substitute(Substitutions, TB),
	atom_concat(File, '.new', NewFile),
	send(TB, save, NewFile),
	send(V, destroy),
	format(user_error, ' done~n', []).
	
substitute(Substitutions, TB) :-
	sort(Substitutions, S1),
	reverse(S1, S2),
	apply_substitutions(S2, TB).

apply_substitutions([], _).
apply_substitutions([H|T], TB) :-
	apply_substitution(H, TB),
	apply_substitutions(T, TB).

apply_substitution(substitute(From, To, New), TB) :-
	Len is To-From,
	send(TB, delete, From, Len),
	send(TB, insert, From, New).

convert_loop(end_of_file, _, _, _) --> !,
	[].
convert_loop(Term, Pos, In, Src) -->
	convert_term(Term, Pos),
	{ fix_syntax(Term, Src),
	  read_term(In, T2, [subterm_positions(P2)]) },
%	{ portray_clause(T2) },
	convert_loop(T2, P2, In, Src).

convert_term(Atom, F-T) --> !,
	(   { atom(Atom), looks_like_regex(Atom) }
	->  { (   regex_emacs_to_advanced(Atom, ADV)
	      ->  true
	      ;   format(user_error, 'failed on ~q~n', [Atom]),
		  Q = Atom
	      ),
	      sformat(Q, '~q', [ADV])
	    },
	    [ substitute(F,T,Q) ]
	;   []
	).
convert_term(_, string_position(_,_)) --> !,
	[].
convert_term({}(A), brace_term_position(_,_,AP)) --> !,
	convert_term(A, AP).
convert_term(List, list_position(_,_,EPs,TP)) --> !,
	convert_term_list(List, EPs, TP).
convert_term(regex(RE), term_position(_,_,_,_,[F-T])) -->
	{ atom(RE), !,
	  (   regex_emacs_to_advanced(RE, ADV)
	  ->  true
	  ;   format(user_error, 'failed on ~q~n', [RE]),
	      Q = RE
	  ),
	  sformat(Q, '~q', [ADV])
	},
	[ substitute(F,T,Q) ].
convert_term(Term, term_position(_,_,_,_,APs)) -->
	{ Term =.. [_|As] },
	convert_term_list(As, APs, none).

convert_term_list([A|AT], [P|PT], T) --> !,
	convert_term(A, P),
	convert_term_list(AT, PT, T).
convert_term_list(_, [], none) --> !,
	[].
convert_term_list(A, [], P) --> !,
	convert_term(A, P).

fix_syntax(Term, _) :-
	requires_library(Term, Lib),
	ensure_loaded(user:Lib),
	fail.
fix_syntax(Term, Src) :-
	catch(expand_term(Term, Expanded), _, Expanded=Term),
	process(Expanded, Src).

process([], _) :- !.
process([H|T], Src) :- !,
	process(H, Src),
	process(T, Src).
process(:- Directive, Src) :- !,
	directive(Directive, Src).
process(_, _).

directive(use_module(Modules), Src) :- !,
	modules(Modules, Src).
directive(pce_expansion:push_compile_operators, _) :- !,
	'$set_source_module'(SM, SM),
	pce_expansion:push_compile_operators(SM).
directive(pce_expansion:pop_compile_operators, _) :- !,
	pce_expansion:pop_compile_operators.
directive(op(P,A,N), _) :- !,
	'$set_source_module'(SM, SM),
	op(P,A,SM:N).
directive(_, _).

modules([], _) :- !.
modules([H|T], Src) :- !,
	modules(H, Src),
	modules(T, Src).
modules(Module, Src) :-
	xref_public_list(Module, _Path, Public, Src),
	'$set_source_module'(SM, SM),
	forall(member(op(P,A,N), Public),
	       op(P,A,SM:N)).


requires_library((:- emacs_begin_mode(_,_,_,_,_)), library(emacs_extend)).
requires_library((:- draw_begin_shape(_,_,_,_)), library(pcedraw)).

%%	looks_like_regex(+Atom)
%	
%	Succeeds if we think Atom is a regumar expression

looks_like_regex(Atom) :-
	re_pattern(Pattern),
	sub_atom(Atom, _,_,_,Pattern), !.

re_pattern('\\(').
re_pattern('\\|').
re_pattern('\\s').
re_pattern('\\S').
re_pattern('\\w').


%%	regex_emacs_to_advanced(+Old, -New)
%	
%	Convert a single regular expression.

regex_emacs_to_advanced(Emacs, Advanced) :-
	atom_codes(Emacs, Codes),
	nb_setval(syntax, emacs),
	phrase(re_compile(Parsed), Codes), !,
	nb_setval(syntax, advanced),
	phrase(re_compile(Parsed), AdvCodes), !,
	atom_codes(Advanced, AdvCodes).


		 /*******************************
		 *	     REGEX DCG		*
		 *******************************/

re_compile(or(B0, B1)) -->
	branch(B0),
	bar,
	re_compile(B1).
re_compile(B) -->
	branch(B).

bar -->
	(   { nb_getval(syntax, emacs) }
	->  "\\|"
	;   "|"
	).


openb -->
	(   { nb_getval(syntax, emacs) }
	->  "\\("
	;   "("
	).


closeb -->
	(   { nb_getval(syntax, emacs) }
	->  "\\)"
	;   ")"
	).


branch([P0|PT]) -->
	piece(P0), !,
	branch(PT).
branch([]) -->
	[].

piece(repeat(Min, Max, Atom)) -->
	atom(Atom),
	qualifier(Min, Max).
piece(Atom) -->
	atom(Atom).
	
qualifier(0, 1) --> "?".
qualifier(0, infinite) --> "*".
qualifier(1, infinite) --> "+".
qualifier(Min, Max) -->
	{ nb_getval(syntax, advanced) },
	"{",
	count(Min),
	(   ","
	->  (   "}"
	    ->	{ Max = infinite }
	    ;	count(Max),
		"}"
	    )
	;   "}",
	    { Max = Min }
	).

atom(char(Atom)) -->
	char(Atom).
atom(char_class(Atom)) -->
	char_class(Atom), !.
atom(constraint(Constraint)) -->
	constraint(Constraint).
atom(backref(N)) -->
	backref(N).
atom(regex(Atom)) -->
	openb, !,
	re_compile(Atom),
	closeb.

constraint(wordsep) -->
	{ nb_getval(syntax, emacs) },
	"\\b".
constraint(wordsep) -->
	{ nb_getval(syntax, advanced) },
	"\\y".

backref(N) -->
	"\\",
	digitval(N).
	
char_class(Atom) -->
	char_class_esc(Atom), !.
char_class(Atom) -->
	char_class_expr(Atom), !.
char_class(Atom) -->
	wild_card_esc(Atom).

char_class_expr(Expr) -->
	"[", !, char_group(Expr), "]".

char_group(and(G0, not(G1))) -->
	{ nb_getval(syntax, advanced) },
	pos_or_neg_char_group(G0), "-",
	char_class_expr(G1).
char_group(G) -->
	pos_or_neg_char_group(G).

pos_or_neg_char_group(not(Group)) -->
	"^", !,
	pos_char_group(Group).
pos_or_neg_char_group(Group) -->
	pos_char_group(Group).

pos_char_group(or(E1, G2)) -->
	char_group_element(E1),
	pos_char_group(G2).
pos_char_group(Group) -->
	char_group_element(Group).

char_group_element(Range) -->
	char_range(Range), !.
char_group_element(Class) -->
	char_class_esc(Class).
char_group_element(char_in(List)) -->	% TBD: escapes for ] and ^
	{ground(List)},
	List.

char(C) -->
	[C],
	\+ { special(C) }, !.
char(C) -->
	single_char_escape(C).

char_range(Range) -->
	se_range(Range).
char_range(char(C)) -->
	xml_char_inc_dash(C).

se_range(range(From,To)) -->
	char_or_esc(From), "-", char_or_esc(To), !.

char_or_esc(C) -->
	xml_char(C), !.
char_or_esc(C) -->
	single_char_escape(C).

xml_char(C) -->
	[C],
	\+ {non_xml_char(C)}.
xml_char_inc_dash(C) -->
	[C],
	\+ {non_xml_char_inc_dash(C)}.

non_xml_char(0'-).
non_xml_char(0'[).
non_xml_char(0']).

non_xml_char_inc_dash(0'[).
non_xml_char_inc_dash(0']).

char_class_esc(char(C)) -->
	single_char_escape(C), !.
char_class_esc(C) -->
	multi_char_esc(C), !.

single_char_escape(C) -->
	"\\", !,
	escape_char(C).

escape_char(0'\n) --> "n".
escape_char(0'\r) --> "r".
escape_char(0'\t) --> "t".
escape_char(C) -->
	[C],
	{ special(C) }.

special(0'.).
special(0'\\).
special(0'?).
special(0'*).	
special(0'+).	
special(0'{) :- nb_getval(syntax, advanced).	
special(0'}) :- nb_getval(syntax, advanced).	
special(0'() :- nb_getval(syntax, advanced).
special(0')) :- nb_getval(syntax, advanced).	
special(0'[).	
special(0']).	
special(0'|) :- nb_getval(syntax, advanced).

wild_card_esc(Set) -->
	".",
	{ Set = not(char_in("\n\r")) }.

multi_char_esc(Set) -->
	{ nb_getval(syntax, advanced) },
	(   wild_card_esc(Set)
	;   adv_multi_char_esc(Set)
	).
multi_char_esc(Set) -->
	{ nb_getval(syntax, emacs) },
	(   wild_card_esc(Set)
	;   emacs_multi_char_esc(Set)
	).

adv_multi_char_esc(char_in(" \t\n\r")) --> "\\s".
adv_multi_char_esc(not(char_in(" \t\n\r"))) --> "\\S".
adv_multi_char_esc(or(letter, char_in("_:"))) --> "\\i".
adv_multi_char_esc(not(or([letter, char_in("_:")]))) --> "\\I".
adv_multi_char_esc(xml_name_char) --> "\\c".
adv_multi_char_esc(not(xml_name_char)) --> "\\C".
adv_multi_char_esc(decimal_digit) --> "\\d".
adv_multi_char_esc(not(decimal_digit)) --> "\\D".
adv_multi_char_esc(not(or(punctuation,separator,other))) --> "\\w".
adv_multi_char_esc(or(punctuation,separator,other)) --> "\\W".
adv_multi_char_esc(newline) --> "\n".
adv_multi_char_esc(upper) --> "[[:upper:]]".
adv_multi_char_esc(not(upper)) --> "[^[:upper:]]".
adv_multi_char_esc(lower) --> "[[:lower:]]".
adv_multi_char_esc(not(lower)) --> "[^[:lower:]]".
adv_multi_char_esc(punct) --> "[[:punct:]]".
adv_multi_char_esc(not(punct)) --> "[^[:punct:]]". % ??

emacs_multi_char_esc(char_in(" \t\n\r")) --> "\\s ".
emacs_multi_char_esc(not(char_in(" \t\n\r"))) --> "\\S ".
emacs_multi_char_esc(decimal_digit) --> "\\sd".
emacs_multi_char_esc(not(decimal_digit)) --> "\\Sd".
emacs_multi_char_esc(char_in("({[")) --> "\\s(".
emacs_multi_char_esc(not(char_in("({["))) --> "\\S(".
emacs_multi_char_esc(newline) --> "\\sn".
emacs_multi_char_esc(upper) --> "\\su".
emacs_multi_char_esc(not(upper)) --> "\\Su".
emacs_multi_char_esc(lower) --> "\\sl".
emacs_multi_char_esc(not(lower)) --> "\\Sl".
emacs_multi_char_esc(punct) --> "\\s.".
emacs_multi_char_esc(not(punct)) --> "\\S.".
emacs_multi_char_esc(not(or(punctuation,separator,other))) --> "\\w".
emacs_multi_char_esc(or(punctuation,separator,other)) --> "\\W".


count(N) -->
	digit(D0),
	digits(D),
	{ catch(number_codes(N, [D0|D]), _, fail)
	}.

digit(D) -->
	[D],
	{ code_type(D, digit) }.

digitval(Val) -->
	[D],
	{ code_type(D, digit(Val)) }.

digits([D0|Ds]) -->
	digit(D0), !,
	digits(Ds).
digits([]) -->
	[].


