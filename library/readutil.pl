/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(read_util,
	  [ read_line_to_codes/2,	% +Fd, -Codes (without trailing \n)
	    read_line_to_codes/3,	% +Fd, -Codes, ?Tail
	    read_stream_to_codes/2,	% +Fd, -Codes
	    read_stream_to_codes/3,	% +Fd, -Codes, ?Tail
	    read_file_to_codes/3,	% +File, -Codes, +Options
	    read_file_to_terms/3	% +File, -Terms, +Options
	  ]).

		 /*******************************
		 *	       LINES		*
		 *******************************/

%	read_line_to_codes(+Fd, -Line)
%
%	Read a line of input from stream into a list of character codes.
%	Trailing newline and or ret are deleted.

read_line_to_codes(Fd, Codes) :-
	get_code(Fd, C0),
	read_1line_to_codes(C0, Fd, Codes0),
	Codes = Codes0.

read_1line_to_codes(-1, _, []) :- !.
read_1line_to_codes(10, _, []) :- !.
read_1line_to_codes(13, Fd, L) :- !,
	get_code(Fd, C2),
	read_1line_to_codes(C2, Fd, L).
read_1line_to_codes(C, Fd, [C|T]) :-
	get_code(Fd, C2),
	read_1line_to_codes(C2, Fd, T).

%	read_line_to_codes(+Fd, -Line, ?Tail)
%
%	Read a line of input as a difference list.  This should be used
%	to read multiple lines efficiently.

read_line_to_codes(Fd, Codes, Tail) :-
	get_code(Fd, C0),
	read_line_to_codes(C0, Fd, Codes0, Tail),
	Codes = Codes0.

read_line_to_codes(-1, _, Tail, Tail) :- !.
read_line_to_codes(10, _, [10|Tail], Tail) :- !.
read_line_to_codes(C, Fd, [C|T], Tail) :-
	get_code(Fd, C2),
	read_line_to_codes(C2, Fd, T, Tail).


		 /*******************************
		 *     STREAM (ENTIRE INPUT)	*
		 *******************************/

%	read_stream_to_codes(+Stream, -Codes, [?Tail]).

read_stream_to_codes(Fd, Codes) :-
	read_stream_to_codes(Fd, Codes, []).
read_stream_to_codes(Fd, Codes, Tail) :-
	get_code(Fd, C0),
	read_stream_to_codes(C0, Fd, Codes0, Tail),
	Codes = Codes0.

read_stream_to_codes(-1, _, Tail, Tail) :- !.
read_stream_to_codes(C, Fd, [C|T], Tail) :-
	get_code(Fd, C2),
	read_stream_to_codes(C2, Fd, T, Tail).


%	read_stream_to_terms(+Stream, -Terms, [?Tail]).

read_stream_to_terms(Fd, Terms) :-
	read_stream_to_terms(Fd, Terms, []).
read_stream_to_terms(Fd, Terms, Tail) :-
	read(Fd, C0),
	read_stream_to_terms(C0, Fd, Terms0, Tail),
	Terms = Terms0.

read_stream_to_terms(end_of_file, _, Tail, Tail) :- !.
read_stream_to_terms(C, Fd, [C|T], Tail) :-
	read(Fd, C2),
	read_stream_to_terms(C2, Fd, T, Tail).


		 /*******************************
		 *     FILE (ENTIRE INPUT)	*
		 *******************************/

%	read_file_to_codes(+Spec, -Codes, +Options)

read_file_to_codes(Spec, Codes, Options) :-
	(   select(tail(Tail), Options, Options1)
	->  true
	;   Tail = [],
	    Options1 = Options
	),
	split_options(Options1, FileOptions, OpenOptions),
	absolute_file_name(Spec,
			   [ access(read)
			   | FileOptions
			   ],
			   Path),
	open(Path, read, Fd, OpenOptions),
	read_stream_to_codes(Fd, Codes0, Tail),
	close(Fd),
	Codes = Codes0.

file_option(extensions(_)).
file_option(file_type(_)).
file_option(file_errors(_)).

split_options([], [], []).
split_options([H|T], File, Open) :-
	(   file_option(H)
	->  File = [H|FT],
	    OT = Open
	;   Open = [H|OT],
	    FT = File
	),
	split_options(T, FT, OT).


%	read_file_to_terms(+Spec, -Terms, +Options)

read_file_to_terms(Spec, Terms, Options) :-
	(   select(tail(Tail), Options, Options1)
	->  true
	;   Tail = [],
	    Options1 = Options
	),
	split_options(Options1, FileOptions, OpenOptions),
	absolute_file_name(Spec,
			   [ access(read)
			   | FileOptions
			   ],
			   Path),
	open(Path, read, Fd, OpenOptions),
	read_stream_to_terms(Fd, Terms0, Tail),
	close(Fd),
	Terms = Terms0.
