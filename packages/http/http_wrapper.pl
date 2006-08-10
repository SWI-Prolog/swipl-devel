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

:- module(httpd_wrapper,
	  [ http_wrapper/5,		% :Goal, +In, +Out, -Conn, +Options
	    http_current_request/1,	% -Request
	    http_relative_path/2	% +AbsPath, -RelPath
	  ]).
:- use_module(http_header).
:- use_module(library(memfile)).
:- use_module(library(lists)).

:- meta_predicate
	http_wrapper(:, +, +, -, +).
:- multifile
	http:request_expansion/2.

%%	http_wrapper(:Goal, +In, +Out, -Close, +Options)
%
%	Simple wrapper to read and decode an HTTP header from `In', call
%	:Goal while watching for exceptions and send the result to the
%	stream `Out'.
%
%	The goal is assumed to write a request to standard output preceeded
%	by a header that should at least contain a Content-type: <type>
%	line.  The header must be closed with a blank line.  The HTTP
%	content-length is added by http_reply/3  Options:
%	
%%	  request(-Request)		Return the request to the caller
%%	  peer(+Peer)			IP address of client

http_wrapper(GoalSpec, In, Out, Close, Options) :-
	strip_module(GoalSpec, Module, Goal),
	wrapper(Module:Goal, In, Out, Close, Options).

wrapper(Goal, In, Out, Close, Options) :-
	http_read_request(In, Request0),
	extend_request(Options, Request0, Request1),
	call_handler(Goal, Request1, Request, Error, MemFile),
	(   var(Error)
	->  size_memory_file(MemFile, Length),
	    open_memory_file(MemFile, read, TmpIn),
	    http_read_header(TmpIn, CgiHeader),
	    join_cgi_header(Request, CgiHeader, Header0),
	    http_update_encoding(Header0, Encoding, Header),
	    set_stream(Out, encoding(Encoding)),
	    (	Encoding == utf8
	    ->  utf8_position_memory_file(MemFile, BytePos, ByteSize),
		Size is ByteSize - BytePos
	    ;   seek(TmpIn, 0, current, Pos),
		Size is Length - Pos
	    ),
	    call_cleanup(reply(TmpIn, Size, Out, Header),
			 cleanup(TmpIn, Out, MemFile)),

	    memberchk(connection(Close), Header)
	;   free_memory_file(MemFile),
	    map_exception(Error, Reply, HdrExtra),
	    http_reply(Reply, Out, HdrExtra),
	    flush_output(Out),
	    (	memberchk(connection(Close), HdrExtra)
	    ->	true
	    ;   Close = close
	    )
	).


%%	call_handler(:Goal, +RequestIn, -RequestOut, -Error, -MemFile)
%	
%	Process RequestIn using Goal, producing CGI data in MemFile

call_handler(Goal, Request0, Request, Error, MemFile) :-
	new_memory_file(MemFile),
	open_memory_file(MemFile, write, TmpOut),
	current_output(OldOut),
	set_output(TmpOut),
	(   catch(call_handler(Goal, Request0, Request), Error, true)
	->  true
	;   Error = failed
	),
	nb_delete(http_request),
	set_output(OldOut),
	close(TmpOut).

call_handler(Goal, Request0, Request) :-
	expand_request(Request0, Request),
	b_setval(http_request, Request),
	call(Goal, Request).

reply(TmpIn, Size, Out, Header) :-
	http_reply(stream(TmpIn, Size), Out, Header),
	flush_output(Out).

cleanup(TmpIn, Out, MemFile) :-
	set_stream(Out, encoding(octet)),
	close(TmpIn),
	free_memory_file(MemFile).


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
%%	http(not_modified)   provides   backward     compatibility    to
%%	http_reply(not_modified).

map_exception(http(not_modified),
	      not_modified,
	      [connection(close)]) :- !.
map_exception(http_reply(Reply),
	      Reply,
	      [connection(close)]) :- !.
map_exception(http_reply(Reply, HdrExtra),
	      Reply,
	      HdrExtra) :- !.
map_exception(E,
	      server_error(E),
	      [connection(close)]).


%%	join_cgi_header(+Request, +CGIHeader, -Header)

join_cgi_header(Request, CgiHeader, [connection(Connect)|Rest]) :-
	select(connection(CgiConn), CgiHeader, Rest), !,
	connection(Request, ReqConnection),
	join_connection(ReqConnection, CgiConn, Connect).
join_cgi_header(Request, CgiHeader, [connection(Connect)|CgiHeader]) :-
	connection(Request, Connect).

join_connection(Keep1, Keep2, Connection) :-
	(   downcase_atom(Keep1, 'keep-alive'),
	    downcase_atom(Keep2, 'keep-alive')
	->  Connection = 'Keep-Alive'
	;   Connection = close
	).


%%	connection(+Header, -Connection)
%	
%	Extract the desired connection from a header.

connection(Header, Close) :-
	(   memberchk(connection(Connection), Header)
	->  Close = Connection
	;   memberchk(http_version(1-X), Header),
	    X >= 1
	->  Close = 'Keep-Alive'
	;   Close = close
	).

%%	extend_request(+Options, +RequestIn, -Request)
%	
%	Merge options in the request.

extend_request([], R, R).
extend_request([request(R)|T], R0, R) :- !,
	extend_request(T, R0, R).
extend_request([peer(P)|T], R0, R) :- !,
	extend_request(T, [peer(P)|R0], R).
extend_request([_|T], R0, R) :- !,
	extend_request(T, R0, R).


%%	http_current_request(-Request)
%	
%	Returns the HTTP request currently being processed.

http_current_request(Request) :-
	b_getval(http_request, Request).


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
