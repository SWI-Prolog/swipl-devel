/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

:- module(httpd_wrapper,
	  [ http_wrapper/5,		% :Goal, +In, +Out, -Conn, +Options
	    http_current_request/1,	% -Request
	    http_send_header/1,		% +Term
	    http_relative_path/2,	% +AbsPath, -RelPath
					% Internal API
	    http_wrap_spawned/3,	% :Goal, -Request, -Connection
	    http_spawned/1		% +ThreadId
	  ]).
:- use_module(http_header).
:- use_module(http_stream).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).

:- meta_predicate
	http_wrapper(0, +, +, -, +).
:- multifile
	http:request_expansion/2.

%%	http_wrapper(:Goal, +In, +Out, -Close, +Options)
%
%	Simple wrapper to read and decode an HTTP header from `In', call
%	:Goal while watching for exceptions and send the result to the
%	stream `Out'.
%
%	The goal is assumed  to  write   the  reply  to =current_output=
%	preceeded by an HTTP header, closed by  a blank line. The header
%	*must* contain a Content-type: <type>   line.  It may optionally
%	contain a line =|Transfer-encoding: chunked|= to request chunked
%	encoding.
%
%	Options:
%	
%		* request(-Request)
%		Return the full request to the caller
%		* peer(+Peer)
%		IP address of client
%		
%	@param Close	Unified to one of =close=, =|Keep-Alife|= or
%			spawned.

http_wrapper(GoalSpec, In, Out, Close, Options) :-
	strip_module(GoalSpec, Module, Goal),
	wrapper(Module:Goal, In, Out, Close, Options).

wrapper(Goal, In, Out, Close, Options) :-
	http_read_request(In, Request0),
	(   Request0 == end_of_file
	->  Close = close,
	    extend_request(Options, [], _) % return request
	;   extend_request(Options, Request0, Request1),
	    memberchk(method(Method), Request1),
	    memberchk(path(Location), Request1),
	    debug(http(wrapper), '~w ~w ...', [Method, Location]),
	    cgi_open(Out, CGI, cgi_hook, [request(Request1)]),
	    cgi_property(CGI, id(Id)),
	    broadcast(http(request_start(Id, Request0))),
	    handler_with_output_to(Goal, Request1, CGI, Error),
	    cgi_close(CGI, Error, Close),
	    debug(http(wrapper), '~w ~w --> ~p', [Method, Location, Error])
	).


%%	http_wrap_spawned(:Goal, -Request, -Close) is det.
%
%	Internal  use  only.  Helper  for    wrapping  the  handler  for
%	http_spawn/2.
%	
%	@see http_spawned/1, http_spawn/2.

http_wrap_spawned(Goal, Request, Close) :-
	handler_with_output_to(Goal, -, current_output, Error),
	(   retract(spawned(_))
	->  Close = spawned,
	    Request = []
	;   current_output(CGI),
	    cgi_property(CGI, request(Request)),
	    cgi_close(CGI, Error, Close)
	).


:- thread_local
	spawned/1.

%%	http_spawned(+ThreadId)
%
%	Internal use only. Indicate that the request is handed to thread
%	ThreadId.

http_spawned(ThreadId) :-
	assert(spawned(ThreadId)).


%%	cgi_close(+CGI, +Error, -Close)

cgi_close(_, _, Close) :-
	retract(spawned(_)), !,
	Close = spawned.
cgi_close(CGI, ok, Close) :- !,
	cgi_property(CGI, connection(Close)),
	close(CGI).
cgi_close(CGI, Error, Close) :-
	cgi_property(CGI, client(Out)),
	cgi_discard(CGI),
	close(CGI),
	map_exception(Error, Reply, HdrExtra),
	http_reply(Reply, Out, HdrExtra),
	flush_output(Out),
	(   memberchk(connection(Close), HdrExtra)
	->  true
	;   Close = close
	).


%%	handler_with_output_to(:Goal, +Request, +Output, -Error) is det.
%
%	Run Goal with output redirected to   Output.  Unifies Error with
%	the output of catch/3 or a term goal_failed(Goal).
%	
%	@param Request	The HTTP request read or '-' for a continuation
%			using http_spawn/2.

handler_with_output_to(Goal, Request, current_output, Error) :- !,
	statistics(cputime, CPU0),
	(   catch(call_handler(Goal, Request), Error, true)
	->  (   var(Error)
	    ->	Error = ok
	    ;	true
	    )
	;   Error = goal_failed(Goal)
	),
	(   spawned(_)
	->  true
	;   statistics(cputime, CPU1),
	    CPU is CPU1 - CPU0,
	    current_output(CGI),
	    cgi_property(CGI, id(Id)),
	    broadcast(http(request_finished(Id, CPU, Error)))
	).
handler_with_output_to(Goal, Request, Output, Error) :-
	current_output(OldOut),
	set_output(Output),
	handler_with_output_to(Goal, Request, current_output, Error),
	set_output(OldOut).

call_handler(Goal, -) :- !,
	call(Goal).
call_handler(Goal, Request0) :-
	expand_request(Request0, Request),
	current_output(CGI),
	cgi_set(CGI, request(Request)),
	call(Goal, Request).


%%	cgi_hook(+Event, +CGI) is det.
%
%	Hook called from the CGI   processing stream. See http_stream.pl
%	for details.

cgi_hook(What, _CGI) :-
	debug(http(hook), 'Running hook: ~q', [What]),
	fail.
cgi_hook(header, CGI) :-
	cgi_property(CGI, header_codes(HeadText)),
	http_parse_header(HeadText, CgiHeader),
	cgi_property(CGI, request(Request)),
	http_update_connection(CgiHeader, Request, Connection, Header1),
	http_update_transfer(Request, Header1, Transfer, Header2),
	http_update_encoding(Header2, Encoding, Header),
	set_stream(CGI, encoding(Encoding)),
	cgi_set(CGI, connection(Connection)),
	cgi_set(CGI, header(Header)),
	debug(http(transfer_encoding), 'Transfer-encoding: ~w', [Transfer]),
	cgi_set(CGI, transfer_encoding(Transfer)). % must be LAST
cgi_hook(send_header, CGI) :-
	cgi_property(CGI, header(Header)),
	cgi_property(CGI, client(Out)),
	(   cgi_property(CGI, transfer_encoding(chunked))
	->  http_reply_header(Out, chunked_data, Header)
	;   cgi_property(CGI, content_length(Len))
	->  http_reply_header(Out, cgi_data(Len), Header)
	).
cgi_hook(close, _).


%%	http_send_header(+Header)
%
%	This API provides an alternative for writing the header field as
%	a CGI header. Header has the  format Name(Value), as produced by
%	http_read_header/2.
%	
%	@deprecated	Use CGI lines instead

http_send_header(Header) :-
	current_output(CGI),
	cgi_property(CGI, header(Header0)),
	cgi_set(CGI, header([Header|Header0])).


%%	expand_request(+Request0, -Request)
%	
%	Allow  for  general   rewrites   of    a   request   by  calling
%	http:request_expansion/2.

expand_request(R0, R) :-
	http:request_expansion(R0, R1),		% Hook
	R1 \== R0, !,
	expand_request(R1, R).
expand_request(R, R).


%%	map_exception(+Exception, -Reply, -HdrExtra)
%	
%	Map certain defined  exceptions  to   special  reply  codes. The
%	http(not_modified)   provides   backward     compatibility    to
%	http_reply(not_modified).

map_exception(http(not_modified),
	      not_modified,
	      [connection('Keep-Alive')]) :- !.
map_exception(http_reply(Reply),
	      Reply,
	      [connection(Close)]) :- !,
	(   keep_alive(Reply)
	->  Close = 'Keep-Alive'
	;   Close = close
	).
map_exception(http_reply(Reply, HdrExtra0),
	      Reply,
	      HdrExtra) :- !,
	(   memberchk(close(_), HdrExtra0)
	->  HdrExtra = HdrExtra0
	;   HdrExtra = [close(Close)|HdrExtra0],
	    (   keep_alive(Reply)
	    ->  Close = 'Keep-Alive'
	    ;   Close = close
	    )
	).
map_exception(error(existence_error(http_location, Location), _),
	      not_found(Location),
	      [connection(close)]) :- !.
map_exception(error(permission_error(http_location, access, Location), _),
	      forbidden(Location),
	      [connection(close)]) :- !.
map_exception(E,
	      resource_error(E),
	      [connection(close)]) :-
	resource_error(E), !.
map_exception(E,
	      server_error(E),
	      [connection(close)]).

resource_error(error(resource_error(_), _)).

%%	keep_alive(+Reply) is semidet.	
%
%	If true for Reply, the default is to keep the connection open.

keep_alive(not_modified).
keep_alive(file(_Type, _File)).
keep_alive(tmp_file(_Type, _File)).
keep_alive(stream(_In, _Len)).
keep_alive(cgi_stream(_In, _Len)).


%%	extend_request(+Options, +RequestIn, -Request)
%	
%	Merge options in the request.

extend_request([], R, R).
extend_request([request(R)|T], R0, R) :- !,
	extend_request(T, R0, R).
extend_request([H|T], R0, R) :-
	request_option(H), !,
	extend_request(T, [H|R0], R).
extend_request([_|T], R0, R) :-
	extend_request(T, R0, R).

request_option(peer(_)).
request_option(protocol(_)).
request_option(pool(_,_,_,_)).


%%	http_current_request(-Request)
%	
%	Returns the HTTP request currently being processed.

http_current_request(Request) :-
	current_output(CGI),
	cgi_property(CGI, request(Request)).


%%	http_relative_path(+AbsPath, -RelPath)
%	
%	Convert an absolute path (without host, fragment or search) into
%	a path relative to the current page.   This  call is intended to
%	create reusable components returning relative   paths for easier
%	support of reverse proxies.

http_relative_path(Path, RelPath) :-
	http_current_request(Request),
	memberchk(path(RelTo), Request),
	http_relative_path(Path, RelTo, RelPath), !.
http_relative_path(Path, Path).

http_relative_path(Path, RelTo, RelPath) :-
	concat_atom(PL, /, Path),
	concat_atom(RL, /, RelTo),
	delete_common_prefix(PL, RL, PL1, PL2),
	to_dot_dot(PL2, DotDot, PL1),
	concat_atom(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
	delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
	to_dot_dot(T0, T, Tail).


		 /*******************************
		 *	    IDE SUPPORT		*
		 *******************************/

% See library('trace/exceptions')

:- multifile
	prolog:general_exception/2.

prolog:general_exception(http_reply(_), http_reply(_)).
prolog:general_exception(http_reply(_,_), http_reply(_,_)).
