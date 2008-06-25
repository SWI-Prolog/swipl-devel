/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_cgi_stream,
	  [ test_cgi_stream/0,
	    t/0, d/0, nd/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(memfile)).
:- use_module(http_stream).
:- use_module(http_header).
:- use_module(http_client).

t :-
	test_cgi_stream.

d :-
	http_stream:http_stream_debug(1),
	debug(http(hook)),
	debug(http(header)).
nd :-
	http_stream:http_stream_debug(0),
	nodebug(http(hook)),
	nodebug(http(header)).

test_cgi_stream :-
	run_tests([ cgi_stream
		  ]).

		 /*******************************
		 *	    DESTINATION		*
		 *******************************/

open_dest(MemF, Out) :-
	new_memory_file(MemF),
	open_memory_file(MemF, write, Out).


http_read_mf(MemF, Header, Data) :-
	open_memory_file(MemF, read, In),
	http_read_reply_header(In, Header),
	http_read_data(Header, Data, to(atom)).


		 /*******************************
		 *	       TEST		*
		 *******************************/

assert_header(Header, Field) :-
	memberchk(Field, Header), !.
assert_header(_Header, Field) :-
	format(user_error, 'ERROR: ~p expected in header~n', [Field]).


		 /*******************************
		 *	      HOOK		*
		 *******************************/

cgi_hook(What, _CGI) :-
	debug(http(hook), 'Running hook: ~q', [What]),
	fail.
cgi_hook(header, CGI) :-
	cgi_property(CGI, header_codes(HeadText)),
	http_parse_header(HeadText, CgiHeader),
	cgi_property(CGI, request(Request)),
	http_update_connection(Request, CgiHeader, Connection, Header1),
	http_update_encoding(Header1, Encoding, Header),
	set_stream(CGI, encoding(Encoding)),
	cgi_set(CGI, connection(Connection)),
	cgi_set(CGI, header(Header)).
cgi_hook(send_header, CGI) :-
	cgi_property(CGI, header(Header)),
	cgi_property(CGI, client(Out)),
	(   cgi_property(CGI, transfer_encoding(chunked))
	->  phrase(http_header:reply_header(chunked_data, Header), String)
	;   cgi_property(CGI, content_length(Len))
	->  phrase(http_header:reply_header(cgi_data(Len), Header), String)
	),
	format(Out, '~s', [String]).
cgi_hook(close, _).


:- begin_tests(cgi_stream, [sto(rational_trees)]).

test(short_text_plain,
     [ Data == Reply,
       setup(open_dest(MemF, Out)),
       cleanup(free_memory_file(MemF))
     ]) :-
	Data = 'Hello world\n',
	cgi_open(Out, CGI, cgi_hook, []),
	format(CGI, 'Content-type: text/plain\n\n', []),
	format(CGI, '~w', [Data]),
	close(CGI),
	close(Out),
	http_read_mf(MemF, Header, Reply),
	assert_header(Header, status(ok, _)).

test(long_unicode_text,
     [ Data == Reply,
       setup(open_dest(MemF, Out)),
       cleanup(free_memory_file(MemF))
     ]) :-
	numlist(1, 10000, L),
	atom_codes(Data, L),
	cgi_open(Out, CGI, cgi_hook, []),
	format(CGI, 'Content-type: text/plain\n\n', []),
	format(CGI, '~w', [Data]),
	close(CGI),
	close(Out),
	http_read_mf(MemF, Header, Reply),
	assert_header(Header, status(ok, _)).

:- end_tests(cgi_stream).

