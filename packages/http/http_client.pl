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


:- module(http_client,
	  [ http_get/3,			% +URL, -Reply, +Options
	    http_post/4,		% +URL, +In, -Reply, +Options
	    http_read_data/3,		% +Header, -Data, +Options
	    http_disconnect/1		% +What
	  ]).
:- use_module(library(socket)).
:- use_module(library(url)).
:- use_module(http_header).
:- use_module(library(debug)).
:- use_module(library(memfile)).
:- use_module(dcg_basics).

:- multifile
	http_convert_data/4.		% http_read_data plugin-hook

:- dynamic
	connection/4.			% Host:Port, ThreadId, In, Out

user_agent('SWI-Prolog (http://www.swi-prolog.org)').

%	connect(+UrlParts, -Read, -Write, +Options)
%	disconnect(+UrlParts)
%
%	Connect/disconnect on the basis of a parsed URL.

connect(Parts, Read, Write, _) :-
	memberchk(socket(Read, Write), Parts), !.
connect(Parts, Read, Write, Options) :-
	address(Parts, Address, Options),
	with_mutex(http_client_connect, connect2(Address, Read, Write)).

connect2(Address, In, Out) :-
	thread_self(Self),
	connection(Address, Self, In, Out), !.
connect2(Address, In, Out) :-
	thread_self(Self),
	do_connect(Address, In, Out),
	assert(connection(Address, Self, In, Out)).

do_connect(Address, In, Out) :-
	tcp_socket(Socket),
	thread_self(Me),
	debug(connection, '~w: Connecting to ~w ...', [Me, Address]),
        tcp_connect(Socket, Address),
	debug(connection, 'ok~n', []),
	tcp_open_socket(Socket, In, Out), !.
do_connect(Address, _, _) :-
	throw(error(failed(connect, Address), _)).
	

disconnect(Parts) :-
	address(Parts, Address, []),
	with_mutex(http_client_connect, disconnect2(Address)).

disconnect2(Address) :-
	thread_self(Me),
	debug(connection, '~w: Closing connection to ~w~n', [Me, Address]),
	thread_self(Self),
	retract(connection(Address, Self, In, Out)), !,
	close_socket(In, Out).

close_socket(In, Out) :-
	close(Out, [force(true)]),
	close(In,  [force(true)]).

%	http_disconnect/1
%	
%	Close down some connections

http_disconnect(all) :-
	(   thread_self(Self),
	    connection(Address, Self, _, _),
	    disconnect(Address),
	    fail
	;   true
	).
	
address(_Parts, Host:Port, Options) :-
	memberchk(proxy(Host, Port), Options), !.
address(Parts, Host:Port, _Options) :-
	memberchk(host(Host), Parts),
	port(Parts, Port).

port(Parts, Port) :-
	memberchk(port(Port), Parts), !.
port(Parts, 80) :-
	memberchk(protocol(http), Parts).

		 /*******************************
		 *	        GET		*
		 *******************************/

%	http_get(+URL, -Data, +Options)
%
%	Get data from an HTTP server.

http_get(URL, Data, Options) :-
	atomic(URL), !,
	parse_url(URL, Parts),
        http_get(Parts, Data, Options).
http_get(Parts, Data, Options) :-
	memberchk(connection(Connection), Options),
	downcase_atom(Connection, 'keep-alive'), !,
	between(0, 1, _),
	catch(http_do_get(Parts, Data, Options), error(io_error, _),
	      (	  format(user_error, 'Error; retrying~n', []),
	          disconnect(Parts),
		  fail
	      )), !.
http_get(Parts, Data, Options) :-
	address(Parts, Address, Options),
	do_connect(Address, Read, Write),
	call_cleanup(http_do_get([socket(Read, Write)|Parts], Data, Options),
		     close_socket(Read, Write)).

http_do_get(Parts, Data, Options) :-
	connect(Parts, Read, Write, Options),
	(   memberchk(proxy(_,_), Options)
	->  parse_url(Location, Parts)
	;   http_location(Parts, Location)
	),
	memberchk(host(Host), Parts),
	http_write_header(Write, 'GET', Location, Host, Options, ReplyOptions),
	write(Write, '\r\n'),
	flush_output(Write),
	http_read_reply(Read, Data, ReplyOptions), !.
http_do_get(Parts, _Data, _Options) :-
	throw(error(failed(http_get, Parts), _)).

http_read_reply(In, Data, Options) :-
	between(0, 1, _),
	    http_read_reply_header(In, Fields),
	\+ memberchk(status(continue, _), Fields), !,
	(   memberchk(reply_header(Fields), Options)
	->  true
	;   true
	),
	http_read_data(In, Fields, Data, Options),
	(   memberchk(connection(Connection), Fields),
	    downcase_atom(Connection, 'keep-alive')
	->  true
	;   thread_self(Self),
	    connection(Address, Self, In, _Out)
	->  disconnect(Address)
	;   true
	).
http_read_reply(In, _Data, _Options) :-
	format(user_error, 'Get FAILED~n', []),
	throw(error(failed(read_reply, In), _)).


%	http_write_header(+Out, +Method, +Location,
%			  +Host, +Options, -RestOptions)
%
%	Write the request header.  It accepts the following options:
%	
%		http_version(Major-Minor)
%		connection(Connection)
%		user_agent(Agent)
%		request_header(Name=Value)
%
%	Remaining options are returned in RestOptions.

http_write_header(Out, Method, Location, Host, Options, RestOptions) :-
	(   select(http_version(Major-Minor), Options, Options1)
	->  true
	;   Major = 1, Minor = 1,
	    Options1 = Options
	),
	format(Out, '~w ~w HTTP/~w.~w\r\n', [Method, Location, Major, Minor]),
	format(Out, 'Host: ~w\r\n', [Host]),
	(   select(connection(Connection), Options1, Options2)
	->  true
	;   Connection = 'Keep-Alive',
	    Options2 = Options1
	),
	(   select(user_agent(Agent), Options2, Options3)
	->  true
	;   user_agent(Agent),
	    Options3 = Options2
	),
	format(Out, 'User-Agent: ~w\r\n\
		     Connection: ~w\r\n', [Agent, Connection]),
	x_headers(Options3, Out, RestOptions).


%	x_headers(+Options, +Out, -RestOptions)
%	
%	Pass additional request options.  For example:
%	
%		request_header('Accept-Language' = 'nl, en')
%		
%	No checking is performed on the fieldname or value. Both are
%	copied literally and in the order of appearance to the request.

x_headers(Options0, Out, Options) :-
	select(request_header(Name=Value), Options0, Options1), !,
	format(Out, '~w: ~w\r\n', [Name, Value]),
	x_headers(Options1, Out, Options).
x_headers(Options, _, Options).

%	http_read_data(+In, +Fields, +Data, -Options)
%
%	If In is a handle to the HTTP server and Fields is the parsed
%	http reply-header, read the reply into Data.  Options is one
%	of:
%
%	to(stream(+WriteStream))
%		Append the content of the message to Stream
%	to(atom)
%		Return the reply as an atom
%	to(codes)
%		Return the reply as a list of codes

http_read_data(Fields, Data, Options) :-
	memberchk(input(In), Fields),
	(   http_read_data(In, Fields, Data, Options)
	->  true
	;   throw(error(failed(http_read_data), _))
	).


http_read_data(In, Fields, Data, Options) :-	% Transfer-encoding: chunked
	select(transfer_encoding(chunked), Fields, RestFields), !,
	(    memberchk(to(stream(Out)), Options)
	->   copy_chunk_data(In, Out, _Foot)
	;    new_memory_file(MemFile),
	     open_memory_file(MemFile, write, Stream),
	     copy_chunk_data(In, Stream, Foot),
	     close(Stream),
	     (	 memberchk(to(atom), Options)
	     ->	 memory_file_to_atom(MemFile, Data0),
		 free_memory_file(MemFile),
		 Data = Data0
	     ;	 memberchk(to(codes), Options)
	     ->	 memory_file_to_codes(MemFile, Data0),
		 free_memory_file(MemFile),
		 Data = Data0
	     ;   size_memory_file(MemFile, Length),
		 append([content_length(Length)|RestFields], Foot, NewFields),
		 open_memory_file(MemFile, read, NewIn),
		 http_read_data(NewIn, NewFields, Data, Options),
		 close(NewIn),
		 free_memory_file(MemFile)
	     )
	).
http_read_data(In, Fields, Data, Options) :-
	memberchk(to(X), Options), !,
	(   X = stream(Stream)
	->  (   memberchk(content_length(Bytes), Fields)
	    ->  copy_stream_data(In, Stream, Bytes)
	    ;   copy_stream_data(In, Stream)
	    )
	;   new_memory_file(MemFile),
	    open_memory_file(MemFile, write, Stream),
	    (   memberchk(content_length(Bytes), Fields)
	    ->  copy_stream_data(In, Stream, Bytes)
	    ;   copy_stream_data(In, Stream)
	    ),
	    close(Stream),
	    (   X == atom
	    ->  memory_file_to_atom(MemFile, Data0)
	    ;	X == codes
	    ->	memory_file_to_codes(MemFile, Data0)
	    ;	throw(error(domain_error(return_type, X), _))
	    ),
	    free_memory_file(MemFile),
	    Data = Data0
	).
http_read_data(In, Fields, Data, _) :-
	memberchk(content_type('application/x-www-form-urlencoded'), Fields), !,
	http_read_data(In, Fields, Codes, [to(codes)]),
	parse_url_search(Codes, Data).
http_read_data(In, Fields, Data, Options) :- 			% call hook
	(   select(content_type(Type), Options, Options1)
	->  delete(Fields, content_type(_), Fields1),
	    http_convert_data(In, [content_type(Type)|Fields1], Data, Options1)
	;   http_convert_data(In, Fields, Data, Options)
	), !.
http_read_data(In, Fields, Data, Options) :-
	http_read_data(In, Fields, Data, [to(atom)|Options]).


%	copy_chunk_data(+In, +Out, -Footer)
%
%	Copy chunk-encoded data (HTTP/1.1, RFC rfc2068) to a new stream
%	and return the foot.

copy_chunk_data(In, Out, Foot) :-
	read_line_to_codes(In, Codes),
	phrase(xinteger(Size), Codes, _), !, 	% _: skip extra header
	(   Size == 0
	->  http_read_header(In, Foot)
	;   copy_stream_data(In, Out, Size),
	    read_line_to_codes(In, ""),		% skip empty line
	    copy_chunk_data(In, Out, Foot)
	).

		 /*******************************
		 *	       POST		*
		 *******************************/

%	http_post(+URL, +In, -Out, +Options)
%
%	Issue an HTTP POST request, In is modelled after the reply
%	from the HTTP server module.  It is one of:
%
%	string(String)
%	string(Type, String)
%	html(Tokens)
%	file(Type, File)

http_post(URL, In, Out, Options) :-
	atomic(URL), !,
	parse_url(URL, Parts),
	http_post(Parts, In, Out, Options).
http_post(Parts, In, Out, Options) :-
	memberchk(connection(Connection), Options),
	downcase_atom(Connection, 'keep-alive'), !,
	between(0, 1, _),
	catch(http_do_post(Parts, In, Out, Options), error(io_error, _),
	      (	  disconnect(Parts),
		  fail
	      )), !.
http_post(Parts, In, Out, Options) :-
	address(Parts, Address, Options),
	do_connect(Address, Read, Write),
	call_cleanup(http_do_post([socket(Read, Write)|Parts],
				  In, Out, Options),
		     close_socket(Read, Write)).

http_do_post(Parts, In, Out, Options) :-
	connect(Parts, Read, Write, Options),
	(   memberchk(proxy(_,_), Options)
	->  parse_url(Location, Parts)
	;   http_location(Parts, Location)
	),
	memberchk(host(Host), Parts),
	split_options(Options, PostOptions, ReplyOptions),
	write_post_header(Write, Location, Host, In, PostOptions),
	http_read_reply(Read, Out, ReplyOptions).

write_post_header(Out, Location, Host, In, Options) :-
	http_write_header(Out, 'POST', Location, Host, Options, DataOptions),
	http_post_data(In, Out, DataOptions),
	flush_output(Out).
	
post_option(connection(_)).
post_option(http_version(_)).
post_option(cache_control(_)).
post_option(request_header(_)).

split_options([], [], []).
split_options([H|T], [H|P], R) :-
	post_option(H), !,
	split_options(T, P, R).
split_options([H|T], P, [H|R]) :-
	split_options(T, P, R).
