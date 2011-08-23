/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

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

:- module(string,
	  [ get_line/2,			% +Stream, -Line
	    get_line/1,			% -Line
	    write_string/2,		% +Stream, +String
	    write_string/1,		% +String
	    whitespace//0,
	    whitespace0//0,
	    string//1			% ?String
	  ]).
:- use_module('../../readutil').

/** <module> CIAO String library

@compat	CIAO Prolog
*/

%%	get_line(+Stream, -Line) is det.
%
%	Reads from Stream a line of text   and unifies Line with it. The
%	end  of  the  line  can  have  UNIX   [10]  or  MS-DOS  [13  10]
%	termination, which is not included  in   Line.  At EOF, the term
%	end_of_file is returned.

get_line(Stream, Line) :-
	read_line_to_codes(Stream, Line).

%%	get_line(-Line) is det.
%
%	Behaves like current_input(S), get_line(S,Line).

get_line(Line) :-
	read_line_to_codes(current_input, Line).

%%	write_string(+Stream, +String) is det.
%
%	Writes String onto Stream.  Output is not flushed.  Is this
%	compatible?

write_string(Stream, String) :-
	format(Stream, '~s', [String]).

%%	write_string(+String) is det.
%
%	Behaves like current_input(S), write_string(S, String).

write_string(String) :-
	format('~s', [String]).

%%	whitespace//
%
%	In a grammar rule,  as   whitespace/0,  represents whitespace (a
%	positive number of space (32), tab   (9), newline (10) or return
%	(13) characters). Thus, Rest is a   proper suffix of String with
%	one or more whitespace characters  removed.   An  example of use
%	would be:
%
%	   ==
%	   attrs([]) --> ""
%	   attrs([N|Ns]) -->
%	       whitespace,
%	       attr(N),
%	       attrs(Ns).
%	   ==

whitespace -->
	[C],
	{ whitespace(C) },
	whitespace0.

%%	whitespace0//
%
%	In  a  grammar  rule,  as   whitespace0/0,  represents  possible
%	whitespace (any number of space (32),   tab (9), newline (10) or
%	return (13) characters). Thus, Rest is String or a proper suffix
%	of String with one or  more   whitespace  characters removed. An
%	example of use would be:
%
%	    ==
%	    assignment(N,V) -->
%	       variable_name(N), whitespace0, "=", whitespace0, value(V).
%	    ==

whitespace0 -->
	[C],
	{ whitespace(C) }, !,
	whitespace.
whitespace0 -->
	[].

whitespace(9).
whitespace(10).
whitespace(13).
whitespace(32).

%%	string(?String)//
%
%	In a grammar rule, as string/1,  represents literally String. An
%	example of use would be:
%
%	    ==
%	    double(A) -->
%	        string(A),
%	        string(A).
%	    ==

string([]) -->
	[].
string([H|T]) -->
	[H],
	string(T).
