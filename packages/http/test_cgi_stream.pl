/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
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
	  [ test_cgi_stream/0
	    , t/0, d/0, nd/0		% Handy things
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).

:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(http_stream).
:- use_module(http_header).
:- use_module(http_client).

/** <module> Test CGI stream object

This module defines a series of tests   outside  the context of the HTTP
server to validate correct  processing  of   the  CGI  header,  handling
different  encodings  and  transfer    encodings  (chunked/traditional).
Instead of using real sockets, we use temporary storage on a file.

@tbd	Validate error processing
*/

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
	run_tests([ cgi_stream,
		    cgi_chunked,
		    cgi_errors
		  ]).

		 /*******************************
		 *	    DESTINATION		*
		 *******************************/

open_dest(TmpF, Out) :-
	tmp_file(http, TmpF),
	open(TmpF, write, Out, [type(binary)]).

free_dest(TmpF) :-
	delete_file(TmpF).

free_dest(TmpF, Out) :-
	close(Out),
	delete_file(TmpF).

http_read_mf(TmpF, Header, Data) :-
	open(TmpF, read, In, [type(binary)]),
	http_read_reply_header(In, Header),
	http_read_data(Header, Data, to(atom)),
	close(In).
      

cat(TmpF) :-
	open(TmpF, read, In),
	call_cleanup(copy_stream_data(In, current_output),
		     close(In)).


		 /*******************************
		 *	      MAKE DATA		*
		 *******************************/

%%	data_atom(+Length, +Min, +Max, -Atom) is det.
%
%	Create an atom of Length codes.  It contains repeating sequences
%	Min..Max.

data_atom(Length, Min, Max, Atom) :-
	data_list(Length, Min, Max, List),
	atom_codes(Atom, List).

data_list(Length, Min, Max, List) :-
	Span is Max - Min,
	data_list(Length, Min, 0, Span, List).

data_list(Len, Min, I, Span, [H|T]) :-
	Len > 0, !,
	H is Min + I mod Span,
	Len2 is Len - 1,
	I2 is I+1,
	data_list(Len2, Min, I2, Span, T).
data_list(_, _, _, _, []).

%%	data(+Name, -Data, -ContentType) is det.
%
%	Create data-sets to be used with the PlUnit forall option.

data(short_ascii, Data, 'text/plain') :-
	data_atom(10, 97, 128, Data).
data(ascii, Data, 'text/plain') :-
	data_atom(126, 1, 128, Data).
data(unicode, Data, 'text/plain') :-
	data_atom(10, 1000, 1010, Data).
data(long_unicode, Data, 'text/plain') :-
	data_atom(10000, 1, 1000, Data).
data(long_binary, Data, 'text/plain') :-
	data_atom(10000, 0, 255, Data).

%%	current_data(-Name) is nondet.
%
%	Enumerate available datasets.

current_data(Name) :-
	clause(data(Name, _, _), _).


		 /*******************************
		 *	       TEST		*
		 *******************************/

assert_header(Header, Field) :-
	memberchk(Field, Header), !.
assert_header(_Header, Field) :-
	format(user_error, 'ERROR: ~p expected in header~n', [Field]),
	fail.


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
	http_update_transfer(Request, Header1, Transfer, Header2),
	http_update_encoding(Header2, Encoding, Header),
	set_stream(CGI, encoding(Encoding)),
	cgi_set(CGI, connection(Connection)),
	cgi_set(CGI, header(Header)),
	cgi_set(CGI, transfer_encoding(Transfer)). % must be LAST
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

test(traditional,
     [ forall(current_data(Name)),
       Reply == Data,
       setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF))
     ]) :-
	data(Name, Data, ContentType),
	cgi_open(Out, CGI, cgi_hook, []),
	format(CGI, 'Content-type: ~w\n\n', [ContentType]),
	format(CGI, '~w', [Data]),
	close(CGI),
	close(Out),
	http_read_mf(TmpF, Header, Reply),
	assert_header(Header, status(ok, _)).

:- end_tests(cgi_stream).

:- begin_tests(cgi_chunked, [sto(rational_trees)]).

test(chunked,
     [ forall(current_data(Name)),
       Reply == Data,
       setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF))
     ]) :-
	data(Name, Data, ContentType),
	cgi_open(Out, CGI, cgi_hook,
 		 [ request([http_version(1-1)])
		 ]),
	format(CGI, 'Transfer-encoding: chunked\n', []),
	format(CGI, 'Content-type: ~w\n\n', [ContentType]),
	format(CGI, '~w', [Data]),
	close(CGI),
	close(Out),
	http_read_mf(TmpF, Header, Reply),
	assert_header(Header, status(ok, _)),
	assert_header(Header, transfer_encoding(chunked)).

:- end_tests(cgi_chunked).


		 /*******************************
		 *	   ERROR HANDLING	*
		 *******************************/

%%	collect_messages(:Goal, -Messages) is semidet.
%
%	Run Goal as once/1, collecting possible messages in Messages.

:- meta_predicate
	collect_messages(0, -, -).

collect_messages(Goal, True, Messages) :-
	strip_module(Goal, M, G),
	collect_messages2(M:G, True, Messages).

:- multifile
	user:message_hook/3.
:- dynamic
	msg_collecting/0,
	msg/2.

user:message_hook(Term, Kind, _Lines) :-
	msg_collecting, !,
	assert(msg(Term, Kind)).

collect_messages2(Goal, True, Messages) :-
	assert(msg_collecting, Ref),
	call_cleanup(call_result(Goal, True),
		     (	 erase(Ref),
			 findall(message(Term, Kind), retract(msg(Term, Kind)),
				 Messages))).

call_result(Goal, true) :-
	Goal, !.
call_result(_, false).


:- begin_tests(cgi_errors, [sto(rational_trees)]).

cgi_fail_hook(Event, _) :-
	debug(http(hook), 'Failing hook for ~w', [Event]),
	fail.

cgi_error_hook(Event, _) :-
	debug(http(hook), 'Error hook for ~w', [Event]),
	throw(error(demo_error, _)).

test(hook_failed,
      [ setup(open_dest(TmpF, Out)),
        cleanup(free_dest(TmpF, Out)),
	error(io_error(_,_))
      ]) :-
 	cgi_open(Out, CGI, cgi_fail_hook, []),
 	close(CGI).

test(hook_error,
     [ setup(open_dest(TmpF, Out)),
       cleanup(free_dest(TmpF, Out)),
       error(demo_error)
     ]) :-
	cgi_open(Out, CGI, cgi_error_hook, []),
	close(CGI).

:- end_tests(cgi_errors).


		 /*******************************
		 *	       PORTRAY		*
		 *******************************/

user:portray(Atom) :-
	atom(Atom),
	atom_length(Atom, Len),
	Len > 100, !,
	sub_atom(Atom, 0, 35, _, Start),
	sub_atom(Atom, _, 35, 0, End),
	format('~q...[~D codes]...~q', [Start, Len, End]).

