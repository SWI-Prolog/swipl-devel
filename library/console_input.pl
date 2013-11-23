/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_console_input,
	  [
	  ]).
:- use_module(library(lists)).

:- multifile
	prolog:complete_input/4.

%%	prolog:complete_input(+BeforeCursor, +AfterCursor,
%%			      -Delete, -Completions) is det.
%
%	Compute    auto    completions    for      the     input    line
%	BeforeCursor+AfterCursor.
%
%	@arg	Delete is an atom or string representing the text that is
%		replaced by the completion
%	@arg	Completions is a list of elements of this shape:
%
%		  - Atom
%		  Used for a plain completion without comment
%		  - Atom-Comment
%		  Used for a completion with comment.  This will be
%		  used for predicates.

prolog:complete_input(Before, _After, Delete, Completions) :-
	string_codes(Before, Chars),
	reverse(Chars, BeforeRev),
	complete(BeforeRev, Delete, Completions).

complete(BeforeRev, Prefix, Files) :-	% complete files
	phrase(file_prefix(Prefix), BeforeRev, _), !,
	atom_concat(Prefix, '*', Pattern),
	expand_file_name(Pattern, Files).
complete(BeforeRev, Prefix, Atoms) :-	% complete atoms
	phrase(atom_prefix(Prefix), BeforeRev, _), !,
	'$atom_completions'(Prefix, Atoms).

%%	atom_prefix(-Prefix) is det.

atom_prefix(Prefix) -->
	atom_chars(RevString),
	{ reverse(RevString, String),
	  string_codes(Prefix, String) % do not create an atom
	}.

atom_chars([H|T]) --> atom_char(H), !, atom_chars(T).
atom_chars([]) --> [].

atom_char(C) --> [C], { atom_char(C) }.

atom_char(C) :- code_type(C, csym).

%%	file_prefix(-Prefix)// is semidet.
%
%	True when the part before the cursor looks like a file name.

file_prefix(Prefix) -->
	file_chars(RevString), "'",
	{ reverse(RevString, String),
	  atom_codes(Prefix, String)
	}.

file_chars([H|T]) --> file_char(H), !, file_chars(T).
file_chars([]) --> [].

file_char(C) --> [C], { file_char(C) }.

file_char(C) :- code_type(C, csym).
file_char(0'/).
file_char(0'.).
file_char(0'-).
file_char(0'~).
:- if(current_prolog_flag(windows,true)).
file_char(0':).
file_char(0'\s).
:- endif.
