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

user_agent('SWI-Prolog (http://www.swi-prolog.org)').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides a simple-minded   light-weight HTTP client library
to get the data from an  URL   using  the GET-method. More advanced HTTP
client support is provided by http_client.pl
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
%		proxy(+Host, +Port)	Use an HTTP proxy server
%		user_agent(+Agent)	User agent for identifying

http_open(Url, Stream, Options) :-
	atom(Url), !,
	parse_url(Url, Parts),
	http_open(Parts, Stream, Options).
http_open(Parts, Stream, Options) :-
	memberchk(proxy(Host, Port), Options), !,
	user_agent(Agent, Options),
	parse_url(URL, Parts),
	open_socket(Host:Port, In, Out, Options),
	format(Out,
	       'GET ~w HTTP/1.0~n\
	       Host: ~w~n\
	       User-Agent: ~w~n\
	       Connection: close~n~n',
	       [URL, Host, Agent]),
	close(Out),
					% read the reply header
	read_header(In, Code, Comment, Lines),
	do_open(Code, Comment, Lines, Options, Parts, In, Stream).
http_open(Parts, Stream, Options) :-
	memberchk(host(Host), Parts),
	option(port(Port), Parts, 80),
	http_location(Parts, Location),
	user_agent(Agent, Options),
	open_socket(Host:Port, In, Out, Options),
	format(Out,
	       'GET ~w HTTP/1.0~n\
	       Host: ~w~n\
	       User-Agent: ~w~n\
	       Connection: close~n~n',
	       [Location, Host, Agent]),
	close(Out),
					% read the reply header
	read_header(In, Code, Comment, Lines),
	do_open(Code, Comment, Lines, Options, Parts, In, Stream).


option(Option, List, Default) :-
	(   memberchk(Option, List)
	->  true
	;   arg(1, Option, Default)
	).

user_agent(Agent, Options) :-
	(   memberchk(user_agent(Agent), Options)
	->  true
	;   user_agent(Agent)
	).

do_open(200, _, Lines, Options, Parts, In, In) :- !,
	return_size(Options, Lines),
					% properly re-initialise the stream
	parse_url(Id, Parts),
	set_stream(In, file_name(Id)),
	set_stream(In, record_position(true)).
					% Handle redirections
do_open(302, _, Lines, Options, _Parts, In, Stream) :-
	location(Lines, Location), !,
	close(In),
	http_open(Location, Stream, Options).
					% report anything else as error
do_open(Code, Comment, _, _, Parts, In, In) :-
	parse_url(Id, Parts),
	throw(error(existence_error(url, Id),
		    context(_, status(Code, Comment)))).


open_socket(Host:Port, In, Out, Options) :-
	tcp_socket(Socket),
	tcp_connect(Socket, Host:Port),
	tcp_open_socket(Socket, In, Out),
	set_stream(In, record_position(false)),
	(   memberchk(Options, timeout(Timeout))
	->  set_stream(In, timeout(Timeout))
	;   true
	).


return_size(Options, Lines) :-
	memberchk(size(Size), Options), !,
	content_length(Lines, Size).
return_size(_, _).


read_header(In, Code, Comment, Lines) :-
	read_line_to_codes(In, Line),
	phrase(first_line(Code, Comment), Line),
	read_line_to_codes(In, Line2),
	rest_header(Line2, In, Lines).


rest_header("", _, []).
rest_header(L0, In, [L0|L]) :-
	read_line_to_codes(In, L1),
	rest_header(L1, In, L).

content_length(Lines, Length) :-
	member(Line, Lines),
	phrase(content_length(Length0), Line), !,
	Length = Length0.

location(Lines, Location) :-
	member(Line, Lines),
	phrase(atom_field("location", Location), Line), !.

first_line(Code, Comment) -->
	"HTTP/", [_], ".", [_],
	skip_blanks,
	integer(Code),
	skip_blanks,
	rest(Comment).

atom_field(Name, Value) -->
	field(Name),
	rest(Value).

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


	
	
