/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
:- use_module(http_exception).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).

:- meta_predicate
	http_wrapper(0, +, +, -, +).
:- multifile
	http:request_expansion/2.

%%	http_wrapper(:Goal, +In, +Out, -Close, +Options) is det.
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
%			spawned(ThreadId).

http_wrapper(Goal, In, Out, Close, Options) :-
	status(Id, State0),
	catch(http_read_request(In, Request0), ReqError, true),
	(   Request0 == end_of_file
	->  Close = close,
	    extend_request(Options, [], _) % return request
	;   var(ReqError)
	->  extend_request(Options, Request0, Request1),
	    memberchk(method(Method), Request1),
	    memberchk(path(Location), Request1),
	    cgi_open(Out, CGI, cgi_hook, [request(Request1)]),
	    cgi_property(CGI, id(Id)),
	    debug(http(request), '[~D] ~w ~w ...', [Id, Method, Location]),
	    handler_with_output_to(Goal, Id, Request1, CGI, Error),
	    cgi_close(CGI, State0, Error, Close)
	;   Id = 0,
	    send_error(Out, State0, ReqError, Close),
	    extend_request(Options, [], _)
	).

status(Id, state0(Thread, CPU, Id)) :-
	thread_self(Thread),
	thread_cputime(CPU).


%%	http_wrap_spawned(:Goal, -Request, -Close) is det.
%
%	Internal  use  only.  Helper  for    wrapping  the  handler  for
%	http_spawn/2.
%
%	@see http_spawned/1, http_spawn/2.

http_wrap_spawned(Goal, Request, Close) :-
	current_output(CGI),
	cgi_property(CGI, id(Id)),
	handler_with_output_to(Goal, Id, -, current_output, Error),
	(   retract(spawned(ThreadId))
	->  Close = spawned(ThreadId),
	    Request = []
	;   cgi_property(CGI, request(Request)),
	    status(Id, State0),
	    catch(cgi_close(CGI, State0, Error, Close),
		  _,
		  Close = close)
	).


:- thread_local
	spawned/1.

%%	http_spawned(+ThreadId)
%
%	Internal use only. Indicate that the request is handed to thread
%	ThreadId.

http_spawned(ThreadId) :-
	assert(spawned(ThreadId)).


%%	cgi_close(+CGI, +State0, +Error, -Close) is det.
%
%	The wrapper has completed. Finish the  CGI output. We have three
%	cases:
%
%	    * The wrapper delegated the request to a new thread
%	    * The wrapper succeeded
%	    * The wrapper threw an error, non-200 status reply
%	    (e.g., =not_modified=, =moved=) or a request to reply with
%	    the content of a file.
%
%	@error socket I/O errors.

cgi_close(_, _, _, Close) :-
	retract(spawned(ThreadId)), !,
	Close = spawned(ThreadId).
cgi_close(CGI, State0, ok, Close) :- !,
	catch(cgi_finish(CGI, Close, Bytes), E, true),
	(   var(E)
	->  http_done(200, ok, Bytes, State0)
	;   http_done(500, E, 0, State0),	% TBD: amount written?
	    throw(E)
	).
cgi_close(CGI, Id, Error, Close) :-
	cgi_property(CGI, client(Out)),
	cgi_discard(CGI),
	close(CGI),
	send_error(Out, Id, Error, Close).

cgi_finish(CGI, Close, Bytes) :-
	flush_output,			% update the content-length
	cgi_property(CGI, connection(Close)),
	cgi_property(CGI, content_length(Bytes)),
	close(CGI).

%%	send_error(+Out, +State0, +Error, -Close)
%
%	Send status replies and  reply   files.  The =current_output= no
%	longer points to the CGI stream, but   simply to the socket that
%	connects us to the client.
%
%	@param	State0 is start-status as returned by status/1.  Used to
%		find CPU usage, etc.

send_error(Out, State0, Error, Close) :-
	map_exception_to_http_status(Error, Reply, HdrExtra),
	catch(http_reply(Reply, Out,
			 [ content_length(CLen)
			 | HdrExtra
			 ],
			 Code),
	      E, true),
	(   var(E)
	->  http_done(Code, Error, CLen, State0)
	;   http_done(500,  E, 0, State0),
	    throw(E)			% is that wise?
	),
	(   memberchk(connection(Close), HdrExtra)
	->  true
	;   Close = close
	).


%%	http_done(+Code, +Status, +BytesSent, +State0) is det.
%
%	Provide feedback for logging and debugging   on  how the request
%	has been completed.

http_done(Code, Status, Bytes, state0(_Thread, CPU0, Id)) :-
	thread_cputime(CPU1),
	CPU is CPU1 - CPU0,
	(   debugging(http(request))
	->  debug_request(Code, Status, Id, CPU, Bytes)
	;   true
	),
	broadcast(http(request_finished(Id, Code, Status, CPU, Bytes))).


%%	handler_with_output_to(:Goal, +Id, +Request, +Output, -Status) is det.
%
%	Run Goal with output redirected to   Output. Unifies Status with
%	=ok=, the error from catch/3  or a term error(goal_failed(Goal),
%	_).
%
%	@param Request	The HTTP request read or '-' for a continuation
%			using http_spawn/2.

handler_with_output_to(Goal, Id, Request, current_output, Status) :- !,
	(   catch(call_handler(Goal, Id, Request), Status, true)
	->  (   var(Status)
	    ->	Status = ok
	    ;	true
	    )
	;   Status = error(goal_failed(Goal),_)
	).
handler_with_output_to(Goal, Id, Request, Output, Error) :-
	current_output(OldOut),
	set_output(Output),
	handler_with_output_to(Goal, Id, Request, current_output, Error),
	set_output(OldOut).

call_handler(Goal, _, -) :- !,		% continuation through http_spawn/2
	call(Goal).
call_handler(Goal, Id, Request0) :-
	expand_request(Request0, Request),
	current_output(CGI),
	cgi_set(CGI, request(Request)),
	broadcast(http(request_start(Id, Request))),
	call(Goal, Request).

%%	thread_cputime(-CPU) is det.
%
%	CPU is the CPU time used by the calling thread.
%
%	@tbd	This does not work on MacOS X!

:- if(current_prolog_flag(threads, true)).
thread_cputime(CPU) :-
	thread_self(Me),
	thread_statistics(Me, cputime, CPU).
:- else.
thread_cputime(CPU) :-
	statistics(cputime, CPU).
:- endif.


%%	cgi_hook(+Event, +CGI) is det.
%
%	Hook called from the CGI   processing stream. See http_stream.pl
%	for details.

cgi_hook(What, _CGI) :-
	debug(http(hook), 'Running hook: ~q', [What]),
	fail.
cgi_hook(header, CGI) :-
	cgi_property(CGI, header_codes(HeadText)),
	cgi_property(CGI, header(Header0)), % see http_send_header/1
	http_parse_header(HeadText, CgiHeader0),
	append(Header0, CgiHeader0, CgiHeader),
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
request_option(pool(_)).


%%	http_current_request(-Request) is semidet.
%
%	Returns  the  HTTP  request  currently  being  processed.  Fails
%	silently if there is no current  request. This typically happens
%	if a goal is run outside the HTTP server context.

http_current_request(Request) :-
	current_output(CGI),
	is_cgi_stream(CGI),
	cgi_property(CGI, request(Request)).


%%	http_relative_path(+AbsPath, -RelPath) is det.
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
	atomic_list_concat(PL, /, Path),
	atomic_list_concat(RL, /, RelTo),
	delete_common_prefix(PL, RL, PL1, PL2),
	to_dot_dot(PL2, DotDot, PL1),
	atomic_list_concat(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
	delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
	to_dot_dot(T0, T, Tail).


		 /*******************************
		 *	   DEBUG SUPPORT	*
		 *******************************/

%%	debug_request(+Code, +Status, +Id, +CPU0, Bytes)
%
%	Emit debugging info after a request completed with Status.

debug_request(Code, ok, Id, CPU, Bytes) :- !,
	debug(http(request), '[~D] ~w OK (~3f seconds; ~D bytes)',
	      [Id, Code, CPU, Bytes]).
debug_request(Code, Status, Id, _, Bytes) :-
	map_exception(Status, Reply), !,
	debug(http(request), '[~D] ~w ~w; ~D bytes',
	      [Id, Code, Reply, Bytes]).
debug_request(Code, Except, Id, _, _) :- !,
	Except = error(_,_), !,
	message_to_string(Except, Message),
	debug(http(request), '[~D] ~w ERROR: ~w',
	      [Id, Code, Message]).
debug_request(Code, Status, Id, _, Bytes) :-
	debug(http(request), '[~D] ~w ~w; ~D bytes',
	      [Id, Code, Status, Bytes]).

map_exception(http_reply(Reply), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
	      error(404, Location)).
