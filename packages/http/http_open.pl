/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(error)).

user_agent('SWI-Prolog (http://www.swi-prolog.org)').

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This library provides a simple-minded   light-weight HTTP client library
to get the data from an  URL   using  the GET-method. More advanced HTTP
client support is provided by http_client.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	http_open(+Url, -Stream, +Options) is det.
%	
%	Open a HTTP url as a (binary) stream. Uses HTTP 1.0 protocol
%	revision to deal with virtual hosts and to be able to interpret
%	the header.  Supported options:
%	
%		* size(-Size)
%		  Return size of the resource
%		* header(Name, -Atom)
%		  Return headerfield as atom
%		* timeout(+Timeout)
%		  Raise exception on timeout
%		* proxy(+Host, +Port)
%		  Use an HTTP proxy server
%		* user_agent(+Agent)
%		  User agent for identifying
%		* request_header(Name=Value)
%		  Extra header field
%		* method(+Method)
%		  One of =get= (default) or =head=
%		* final_url(-URL)
%		  Unified with the final URL to deal with
%		  redirections.
%	
%	@error	existence_error(url, Id)

http_open(URL, Stream, Options) :-
	atom(URL), !,
	parse_url(URL, Parts),
	http_open(Parts, Stream, Options).
http_open(Parts, Stream, Options0) :-
	Options = [visited(Parts)|Options0],
	memberchk(proxy(Host, ProxyPort), Options), !,
	parse_url(Location, Parts),
	open_socket(Host:ProxyPort, In, Out, Options),
	option(port(Port), Parts, 80),
	host_and_port(Host, Port, HostPort),
	send_rec_header(Out, In, Stream, HostPort, Location, Parts, Options),
	return_final_url(Options).
http_open(Parts, Stream, Options0) :-
	Options = [visited(Parts)|Options0],
	memberchk(host(Host), Parts),
	option(port(Port), Parts, 80),
	http_location(Parts, Location),
	open_socket(Host:Port, In, Out, Options),
	host_and_port(Host, Port, HostPort),
	send_rec_header(Out, In, Stream, HostPort, Location, Parts, Options),
	return_final_url(Options).

host_and_port(Host, 80, Host) :- !.
host_and_port(Host, Port, Host:Port).

%%	send_rec_header(+Out, +In, -InStream,
%%			+Host, +Location, +Parts, +Options) is det.
%
%	Send header to Out and process reply.  If there is an error or
%	failure, close In and Out and return the error or failure.

send_rec_header(Out, In, Stream, Host, Location, Parts, Options) :-
	(   catch(guarded_send_rec_header(Out, In, Stream,
					  Host, Location, Parts, Options),
		  E, true)
	->  (   var(E)
	    ->	close(Out)
	    ;	force_close(In, Out),
		throw(E)
	    )
	;   force_close(In, Out),
	    fail
	).

guarded_send_rec_header(Out, In, Stream, Host, Location, Parts, Options) :-
	user_agent(Agent, Options),
	method(Options, MNAME),
	format(Out,
	       '~w ~w HTTP/1.0\r\n\
	       Host: ~w\r\n\
	       User-Agent: ~w\r\n\
	       Connection: close\r\n',
	       [MNAME, Location, Host, Agent]),
	x_headers(Options, Out, Options1),
	format(Out, '\r\n', []),
	flush_output(Out),
					% read the reply header
	read_header(In, Code, Comment, Lines),
	do_open(Code, Comment, Lines, Options1, Parts, In, Stream).

force_close(S1, S2) :-
	close(S1, [force(true)]),
	close(S2, [force(true)]).

method(Options, MNAME) :-
	option(method(M), Options, get),
	must_be(oneof([get,head]), M),
	(   M == get
	->  MNAME = 'GET'
	;   MNAME = 'HEAD'
	).

x_headers(Options0, Out, Options) :-
	select(request_header(Name=Value), Options0, Options1), !,
	format(Out, '~w: ~w\r\n', [Name, Value]),
	x_headers(Options1, Out, Options).
x_headers(Options, _, Options).


user_agent(Agent, Options) :-
	(   option(user_agent(Agent), Options)
	->  true
	;   user_agent(Agent)
	).

do_open(200, _, Lines, Options, Parts, In, In) :- !,
	return_size(Options, Lines),
	return_fields(Options, Lines),
					% properly re-initialise the stream
	parse_url(Id, Parts),
	set_stream(In, file_name(Id)),
	set_stream(In, record_position(true)).
					% Handle redirections
do_open(Code, _, Lines, Options, Parts, In, Stream) :-
	redirect_code(Code),
	location(Lines, Location), !,
	parse_url(Location, Parts, Redirected),
	close(In),
	http_open(Redirected, Stream, [visited(Redirected)|Options]).
					% report anything else as error
do_open(Code, Comment, _, _, Parts, In, In) :-
	close(In),
	parse_url(Id, Parts),
	throw(error(existence_error(url, Id),
		    context(_, status(Code, Comment)))).

redirect_code(301).			% moved permanently
redirect_code(302).			% moved temporary
redirect_code(303).			% see also

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

return_fields([], _).
return_fields([header(Name, Value)|T], Lines) :- !,
	atom_codes(Name, Codes),
	(   member(Line, Lines),
	    phrase(atom_field(Codes, Value), Line)
	->  true
	;   Value = ''
	),
	return_fields(T, Lines).
return_fields([_|T], Lines) :-
	return_fields(T, Lines).


%%	return_final_url(+Options) is semidet.
%
%	If Options contains final_url(URL), unify URL with the final
%	URL after redirections.

return_final_url(Options) :-
	memberchk(final_url(URL), Options),
	var(URL), !,
	memberchk(visited(Parts), Options),
	parse_url(URL, Parts).
return_final_url(_).

	
%%	read_header(+In:stream, -Code:int, -Comment:atom, -Lines:list)
%
%	Read the HTTP reply-header.
%	
%	@param Code	Numeric HTTP reply-code
%	@param Comment	Comment of reply-code as atom
%	@param Lines	Remaining header lines as code-lists.

read_header(In, Code, Comment, Lines) :-
	read_line_to_codes(In, Line),
	phrase(first_line(Code, Comment), Line),
	read_line_to_codes(In, Line2),
	rest_header(Line2, In, Lines).

rest_header("", _, []) :- !.		% blank line: end of header
rest_header(L0, In, [L0|L]) :-
	read_line_to_codes(In, L1),
	rest_header(L1, In, L).

%%	content_length(+Header, -Length:int) is semidet.
%
%	Find the Content-Length in an HTTP reply-header.

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
	{ match_header_char(H, C)
	},
	field(T).

match_header_char(C, C) :- !.
match_header_char(C, U) :-
	code_type(C, to_lower(U)), !.
match_header_char(0'_, 0'-).


skip_blanks -->
	[C],
	{ code_type(C, white)
	}, !,
	skip_blanks.
skip_blanks -->
	[].

%%	integer(-Int)//
%
%	Read 1 or more digits and return as integer.

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

%%	rest(-Atom:atom)//
%
%	Get rest of input as an atom.

rest(A,L,[]) :-
	atom_codes(A, L).


	
	
