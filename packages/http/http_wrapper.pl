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
	  [ http_wrapper/5	% :Goal, +In, +Out, -Connection, +Options
	  ]).
:- use_module(http_header).
:- use_module(library(memfile)).

:- meta_predicate
	http_wrapper(:, +, +, -, +).

%	http_wrapper(:Goal, +In, +Out, -Close, +Options)
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
%	  request(-Request)		Return the request to the caller

http_wrapper(Goal, In, Out, Close, Options) :-
	http_read_request(In, Request),
	(   memberchk(request(Request), Options)
	->  true
	;   true
	),
	new_memory_file(MemFile),
	open_memory_file(MemFile, write, TmpOut),
	current_output(OldOut),
	set_output(TmpOut),
	(   catch(call(Goal, Request), E, true)
	->  true
	;   E = failed
	),
	set_output(OldOut),
	close(TmpOut),
	(   var(E)
	->  size_memory_file(MemFile, Length),
	    open_memory_file(MemFile, read, TmpIn),
	    http_read_header(TmpIn, CgiHeader),
	    seek(TmpIn, 0, current, Pos),
	    Size is Length - Pos,
	    join_cgi_header(Request, CgiHeader, Header),
	    http_reply(stream(TmpIn, Size), Out, Header),
	    flush_output(Out),
	    close(TmpIn),
	    free_memory_file(MemFile),
	    memberchk(connection(Close), Header)
	;   free_memory_file(MemFile),
	    map_exception(E, Reply, HdrExtra),
	    http_reply(Reply, Out, HdrExtra),
	    flush_output(Out),
	    (	memberchk(connection(Close), HdrExtra)
	    ->	true
	    ;   Close = close
	    )
	).

%	map_exception(+Exception, -Reply, -HdrExtra)
%	
%	Map certain defined  exceptions  to   special  reply  codes. The
%	http(not_modified)   provides   backward     compatibility    to
%	http_reply(not_modified).

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


%	connection(+Header, -Connection)
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
