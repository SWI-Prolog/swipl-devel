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

:- module(demo_body,
	  [ reply/1
	  ]).
:- use_module(library('http/http_client')).
:- use_module(library('http/http_mime_plugin')). % Decode multipart data
:- use_module(library('http/http_image')).	 % make XPCE generate images


:- style_check(-atom).			% allow long atoms

reply(_) :-
	flag(request, N, N+1),
	fail.

%	/quit
%	
%	Explicitely close the connection

reply(Request) :-
	member(path('/quit'), Request), !,
	format('Connection: close~n', []),
	format('Content-type: text/html~n~n', []),
	format('Bye Bye~n').

%	/xpce?class=box
%	
%	Make XPCE reply with a graphics image. The demo-body pce_reply/1
%	is called embedded in a  message  to   XPCE  to  force  the XPCE
%	incremental garbage collector to reclaim   objects created while
%	serving the request. pce_reply/1 replies   to ?class=box using a
%	blue box with rounded corners.

reply(Request) :-
	member(path('/xpce'), Request), !,
	send(@prolog, call, demo_body:pce_reply(Request)).

%	/env
%	
%	Reply with the output of printenv (Unix systems only).

reply(Request) :-
	member(path('/env'), Request), !,
	expand_file_name(~, Home),
	format('Content-type: text/html~n~n', []),
	format('<html>~n', []),
	flag(request, RN, RN),
	format('Request ~d~n', [RN]),
	format('<pre>~n', []),
	format('HOME = ~w~n~n', [Home]),
	open(pipe(printenv), read, Fd),
	copy_stream_data(Fd, current_output),
	close(Fd),
	format('</pre>~n', []),
	format('</html>~n', []).

%	/upload
%	/upload_reply
%	
%	Provide a form for uploading a file, and deal with the resulting
%	upload.  Contributed by Nicos Angelopoulos.

reply(Request) :-
        member(path('/upload'), Request), !,
        format('Content-type: text/html~n~n', []),
        format('<html>~n', []),
	format('<form action="/upload_reply" enctype="multipart/form-data" method="post">~n', []),
	format('<input type="file" name="datafile">'),
	format('<input type="submit" name="sent">'),
        format('</body>~n', []),
        format('</html>~n', []).

reply(Request) :-
        member(path('/upload_reply'), Request), !,
        format('Content-type: text/html~n~n', []),
        format('<html>~n', []),
        format('<pre>~n', []),
	write( req(Request) ), nl,
	http_read_data(Request, Data, []),
	write( data(Data) ), nl,
	format('</pre>'),
        format('</body>~n', []),
        format('</html>~n', []).

%	/xml
%	
%	Return a simple formatted XML message.

reply(Request) :-
	member(path('/xml'), Request), !,
	format('Content-type: text/xml~n~n', []),
	format('\
<message>
  <head>
  <from>Jan Wielemaker</from>
  <to>Prolog users</to>
  <subject>The SWI-Prolog web-server</subject>
  </head>
  <body>
<p>
This is the first demo of the web-server serving an XML message
</p>
  </body>
</message>
', []).

%	/foreign
%	
%	Test emitting text using UTF-8 encoding

reply(Request) :-
	member(path('/foreign'), Request), !,
	format('Content-type: text/html~n~n', []),
	format('\
<html>
<head><title>Foreign characters</title></head>
<body>
<p>Chinese for book is ~s
</body>
</html>
',
[ [23398, 20064]
]).


%	/work
%	
%	Do a lot of work and then say 'ok'. Can be used to test
%	concurrent access using the multi-threaded server.

reply(Request) :-
	member(path('/work'), Request),
	format(user_error, 'Starting work ...', []),
	forall(between(1, 10000000, _), atom_codes(_, "hello")),
	format(user_error, 'done!~n', []),
	format('Content-type: text/plain~n~n', []),
	format('ok~n').

%	... Otherwise
%	
%	Print the request itself.

reply(Request) :-
	format('Content-type: text/html~n~n', []),
	format('<html>~n', []),
	format('<table border=1>~n'),
	print_request(Request),
	format('~n</table>~n'),
	format('</html>~n', []).


print_request([]).
print_request([H|T]) :-
	H =.. [Name, Value],
	format('<tr><td>~w<td>~w~n', [Name, Value]),
	print_request(T).


		 /*******************************
		 *     PCE BASED REQUESTS	*
		 *******************************/

pce_reply(Request) :-
	memberchk(search(Search), Request),
	memberchk(class=box, Search),
	new(Box, box(200,200)),
	send(Box, radius, 20),
	send(Box, fill_pattern, colour(skyblue)),
	reply_image(Box, []).
	
	

