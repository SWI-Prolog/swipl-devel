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


:- module(http_open,
	  [ http_open/3			% +URL, -Stream, +Options
	  ]).
:- use_module(library(url)).
:- use_module(library(readutil)).
:- use_module(library(socket)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides a simple-minded light-weight HTTP client library
to get the data from an URL using the GET-method. 

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%	http_open(+Url, -Stream, [+Options])
%	
%	Open a HTTP url as a (binary) stream. Uses HTTP 1.0 protocol
%	revision to deal with virtual hosts and to be able to interpret
%	the header.
%	
%	Supported options:
%	
%		size(-Size)		Return size of the resource
%		timeout(+Timeout)	Raise exception on timeout

http_open(Url, Stream, Options) :-
	atom(Url), !,
	parse_url(Url, Parts),
	http_open(Parts, Stream, Options).
http_open(Parts, Stream, Options) :-
	memberchk(host(Host), Parts),
	(   memberchk(port(Port), Parts)
	->  true
	;   Port = 80
	),
	http_location(Parts, Location),
	tcp_socket(Socket),
	tcp_connect(Socket, Host:Port),
	tcp_open_socket(Socket, In, Out),
	set_stream(In, record_position(false)),
	(   memberchk(Options, timeout(Timeout))
	->  set_stream(In, timeout(Timeout))
	;   true
	),
	format(Out,
	       'GET ~w HTTP/1.0~n\
	       Host: ~w~n\
	       Connection: close~n~n',
	       [Location, Host]),
	close(Out),
					% read the reply header
	read_header(In, Code, Comment, Size),
					% check the status
	report_status(Code, Id, Comment),
					% fill options
	(   memberchk(size(ReqSize), Options)
	->  (   ReqSize = Size
	    ->	true
	    ;	close(Out),
		fail
	    )
	;   true
	),
					% properly re-initialise the stream
	parse_url(Id, Parts),
	set_stream(In, file_name(Id)),
	set_stream(In, record_position(true)),
	Stream = In.


report_status(200, _, _) :- !.
report_status(_, Id, Comment) :-
	throw(error(existence_error(url, Id),
		    context(_, Comment))).


read_header(In, Code, Comment, Size) :-
	read_line_to_codes(In, Line),
	phrase(first_line(Code, Comment), Line),
	read_line_to_codes(In, Line2),
	rest_header(Line2, In, 0, Size).


rest_header("", _, Size, Size).
rest_header(Line, In, _, Size) :-
	phrase(content_length(Size0), Line), !,
	read_line_to_codes(In, Line2),
	rest_header(Line2, In, Size0, Size).
rest_header(_, In, Size0, Size) :-
	read_line_to_codes(In, Line2),
	rest_header(Line2, In, Size0, Size).



first_line(Code, Comment) -->
	"HTTP/", [_], ".", [_],
	skip_blanks,
	integer(Code),
	skip_blanks,
	rest(Comment).


content_length(Len) -->
	field("content-length"),
	integer(Len).

field([]) -->
	":",
	skip_blanks.
field([H|T]) -->
	[C],
	{ code_type(H, to_lower(C))
	},
	field(T).


skip_blanks -->
	[C],
	{ code_type(C, white)
	}, !,
	skip_blanks.
skip_blanks -->
	[].


integer(Code) -->
	digit(D0),
	digits(D),
	{ number_codes(Code, [D0|D])
	}.


digit(C) -->
	[C],
	{ code_type(C, digit)
	}.


digits([D0|D]) -->
	digit(D0), !,
	digits(D).
digits([]) -->
	[].


rest(A,L,[]) :-
	atom_codes(A, L).


	
	
