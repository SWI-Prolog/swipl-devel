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

:- module(http_header,
	  [ http_read_request/2,	% +Stream, -Request
	    http_read_reply_header/2,	% +Stream, -Reply
	    http_reply/2,		% +What, +Stream
	    http_reply/3,		% +What, +Stream, +HdrExtra

	    http_post_data/3,		% +Stream, +Data, +HdrExtra

	    http_read_header/2,		% +Fd, -Header
	    http_join_headers/3		% +Default, +InHdr, -OutHdr
	  ]).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(dcg_basics).
:- use_module(html_write).
:- use_module(mimetype).
:- use_module(mimepack).


		 /*******************************
		 *	    READ REQUEST	*
		 *******************************/

%	http_read_request(+FdIn, -Request)
%
%	Read an HTTP request-header from FdIn and return the broken-down
%	request fields as +Name(+Value) pairs in a list.

http_read_request(In, [input(In)|Request]) :-
	read_line_to_codes(In, Codes),
	debug(header, 'First line: ~s~n', [Codes]),
	phrase(request(In, Request), Codes).

%	http_read_reply_header(+FdIn, -Reply)
%
%	Read the HTTP reply header (if any).

http_read_reply_header(In, [input(In)|Reply]) :-
	read_line_to_codes(In, Codes),
	debug(header, 'First line: ~s~n', [Codes]),
	(   phrase(reply(In, Reply), Codes)
	->  true
	;   throw(error(syntax(http_reply_header, Codes), _))
	).


		 /*******************************
		 *	  FORMULATE REPLY	*
		 *******************************/

http_reply(What, Out) :-
	http_reply(What, Out, [connection(close)]).

http_reply(html(HTML), Out, HrdExtra) :- !,
	phrase(reply_header(html(HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(file(Type, File), Out, HrdExtra) :- !,
	phrase(reply_header(file(Type, File), HrdExtra), Header),
	format(Out, '~s', [Header]),
	open(File, read, In),
	copy_stream_data(In, Out),
	close(In).
http_reply(tmp_file(Type, File), Out, HrdExtra) :- !,
	phrase(reply_header(tmp_file(Type, File), HrdExtra), Header),
	format(Out, '~s', [Header]),
	open(File, read, In),
	copy_stream_data(In, Out),
	close(In).
http_reply(stream(In, Len), Out, HdrExtra) :- !,
	phrase(reply_header(cgi_data(Len), HdrExtra), Header),
	format(Out, '~s', [Header]),
	copy_stream_data(In, Out).
http_reply(cgi_stream(In, Len), Out, HrdExtra) :- !,
	http_read_header(In, CgiHeader),
	seek(In, 0, current, Pos),
	Size is Len - Pos,
	http_join_headers(HrdExtra, CgiHeader, Hdr2),
	phrase(reply_header(cgi_data(Size), Hdr2), Header),
	format(Out, '~s', [Header]),
	copy_stream_data(In, Out).
http_reply(moved(To), Out, HrdExtra) :- !,
	phrase(page([ title('301 Moved Permanently')
		    ],
		    [ h1('Moved Permanently'),
		      p(['The document has moved ',
			 a(href(To), ' Here')
			]),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(moved(To, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(not_found(URL), Out, HrdExtra) :- !,
	phrase(page([ title('404 Not Found')
		    ],
		    [ h1('Not Found'),
		      p(['The requested URL ', URL,
			 ' was not found on this server'
			]),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(status(not_found, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(forbidden(URL), Out, HrdExtra) :- !,
	phrase(page([ title('403 Forbidden')
		    ],
		    [ h1('Forbidden'),
		      p(['You don\'t have permission to access ', URL,
			 ' on this server'
			]),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(status(forbidden, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(authorise(Method, Realm), Out, HrdExtra) :- !,
	phrase(page([ title('401 Authorization Required')
		    ],
		    [ h1('Authorization Required'),
		      p(['This server could not verify that you ',
			 'are authorized to access the document ',
			 'requested.  Either you supplied the wrong ',
			 'credentials (e.g., bad password), or your ',
			 'browser doesn\'t understand how to supply ',
			 'the credentials required.'
			]),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(authorise(Method, Realm, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(not_modified, Out, HrdExtra) :- !,
	phrase(page([ title('304 Not Modified')
		    ],
		    [ h1('Not Modified'),
		      p(['The resource has not changed']),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(status(not_modified, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(server_error(ErrorTerm), Out, HrdExtra) :-
	message_to_html(ErrorTerm, Tokens),
	phrase(page([ title('500 Internal server error')
		    ],
		    [ h1('Internal server error'),
		      p(Tokens),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(status(server_error, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).



message_to_html(Term, Tokens) :-
	'$messages':translate_message(Term, Lines, []),
	phrase(html_message_lines(Lines), Tokens).

html_message_lines([]) -->
	[].
html_message_lines([nl|T]) --> !,
	['<br>'],
	html_message_lines(T).
html_message_lines([flush]) -->
	[].
html_message_lines([Fmt-Args|T]) --> !,
	{ sformat(S, Fmt, Args)
	},
	[S],
	html_message_lines(T).
html_message_lines([Fmt|T]) --> !,
	{ sformat(S, Fmt, [])
	},
	[S],
	html_message_lines(T).

%	http_join_headers(+Default, +Header, -Out)
%	Append headers from Default to Header if they are not
%	already part of it.

http_join_headers([], H, H).
http_join_headers([H|T], Hdr0, Hdr) :-
	functor(H, N, A),
	functor(H2, N, A),
	member(H2, Hdr0), !,
	http_join_headers(T, Hdr0, Hdr).
http_join_headers([H|T], Hdr0, [H|Hdr]) :-
	http_join_headers(T, Hdr0, Hdr).

		 /*******************************
		 *	    POST SUPPORT	*
		 *******************************/

http_post_data(html(HTML), Out, HdrExtra) :-
	phrase(post_header(html(HTML), HdrExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_post_data(file(File), Out, HdrExtra) :- !,
	(   file_mime_type(File, Type)
	->  true
	;   Type = text/plain
	),
	http_post_data(file(Type, File), Out, HdrExtra).
http_post_data(file(Type, File), Out, HdrExtra) :- !,
	phrase(post_header(file(Type, File), HdrExtra), Header),
	format(Out, '~s', [Header]),
	open(File, read, In),
	copy_stream_data(In, Out),
	close(In).
http_post_data(cgi_stream(In, Len), Out, HdrExtra) :- !,
	http_read_header(In, CgiHeader),
	seek(In, 0, current, Pos),
	Size is Len - Pos,
	http_join_headers(HdrExtra, CgiHeader, Hdr2),
	phrase(reply_header(cgi_data(Size), Hdr2), Header),
	format(Out, '~s', [Header]),
	copy_stream_data(In, Out, Size).
http_post_data(form(Fields), Out, HdrExtra) :- !,
	parse_url_search(Codes, Fields),
	length(Codes, Size),
	http_join_headers(HdrExtra,
			  [ content_type('application/x-www-form-urlencoded')
			  ], Header),
	phrase(post_header(cgi_data(Size), Header), HeaderChars),
	format(Out, '~s', [HeaderChars]),
	format(Out, '~s', [Codes]).
http_post_data(form_data(Data), Out, HdrExtra) :- !,
	new_memory_file(MemFile),
	open_memory_file(MemFile, write, MimeOut),
	mime_pack(Data, MimeOut, Boundary),
	close(MimeOut),
	size_memory_file(MemFile, Size),
	sformat(ContentType, 'multipart/form-data; boundary=~w', [Boundary]),
	http_join_headers(HdrExtra,
			  [ mime_version('1.0'),
			    content_type(ContentType)
			  ], Header),
	phrase(post_header(cgi_data(Size), Header), HeaderChars),
	format(Out, '~s', [HeaderChars]),
	open_memory_file(MemFile, read, In),
	copy_stream_data(In, Out),
	close(In),
	free_memory_file(MemFile).
http_post_data(List, Out, HdrExtra) :-		% multipart-mixed
	is_list(List), !,
	new_memory_file(MemFile),
	open_memory_file(MemFile, write, MimeOut),
	mime_pack(List, MimeOut, Boundary),
	close(MimeOut),
	size_memory_file(MemFile, Size),
	sformat(ContentType, 'multipart/mixed; boundary=~w', [Boundary]),
	http_join_headers(HdrExtra,
			  [ mime_version('1.0'),
			    content_type(ContentType)
			  ], Header),
	phrase(post_header(cgi_data(Size), Header), HeaderChars),
	format(Out, '~s', [HeaderChars]),
	open_memory_file(MemFile, read, In),
	copy_stream_data(In, Out),
	close(In),
	free_memory_file(MemFile).

%	post_header//2: DCG for generating the POST request

post_header(html(Tokens), HdrExtra) -->
	header_fields(HdrExtra),
	content_length(html(Tokens)),
	content_type(text/html),
	"\r\n". 
post_header(file(Type, File), HdrExtra) -->
	header_fields(HdrExtra),
	content_length(file(File)),
	content_type(Type),
	"\r\n". 
post_header(cgi_data(Size), HdrExtra) -->
	header_fields(HdrExtra),
	content_length(Size),
	"\r\n". 


		 /*******************************
		 *       OUTPUT HEADER DCG	*
		 *******************************/

reply_header(What) -->
	reply_header(What, []).

reply_header(string(String), HdrExtra) -->
	reply_header(string(text/plain, String), HdrExtra).
reply_header(string(Type, String), HdrExtra) -->
	vstatus(ok),
	date(now),
	header_fields(HdrExtra),
	content_length(ascii_string(String)),
	content_type(Type),
	"\r\n".
reply_header(html(Tokens), HdrExtra) -->
	vstatus(ok),
	date(now),
	header_fields(HdrExtra),
	content_length(html(Tokens)),
	content_type(text/html),
	"\r\n".
reply_header(file(Type, File), HdrExtra) -->
	vstatus(ok),
	date(now),
	modified(file(File)),
	header_fields(HdrExtra),
	content_length(file(File)),
	content_type(Type),
	"\r\n".
reply_header(tmp_file(Type, File), HdrExtra) -->
	vstatus(ok),
	date(now),
	header_fields(HdrExtra),
	content_length(file(File)),
	content_type(Type),
	"\r\n".
reply_header(cgi_data(Size), HdrExtra) -->
	vstatus(ok),
	date(now),
	header_fields(HdrExtra),
	content_length(Size),
	"\r\n".
reply_header(moved(To, Tokens), HdrExtra) -->
	vstatus(moved),
	date(now),
	header_field('Location', To),
	header_fields(HdrExtra),
	content_length(html(Tokens)),
	content_type(text/html),
	"\r\n".
reply_header(status(Status, Tokens), HdrExtra) -->
	vstatus(Status),
	date(now),
	header_fields(HdrExtra),
	content_length(html(Tokens)),
	content_type(text/html),
	"\r\n".
reply_header(authorise(Method, Realm, Tokens), HdrExtra) -->
	vstatus(authorise),
	date(now),
	authenticate(Method, Realm),
	header_fields(HdrExtra),
	content_length(html(Tokens)),
	content_type(text/html),
	"\r\n".

vstatus(Status) -->
	"HTTP/1.1 ",
	status_number(Status),
	" ",
	status_comment(Status),
	"\r\n".

status_number(continue)	    --> "100".
status_number(ok)	    --> "200".
status_number(moved)	    --> "301".
status_number(not_modified) --> "304". 
status_number(not_found)    --> "404".
status_number(forbidden)    --> "403".
status_number(authorise)    --> "401".
status_number(server_error) --> "500".

status_comment(continue) -->
	"Continue".
status_comment(ok) -->
	"OK".
status_comment(moved) -->
	"Moved Permanently".
status_comment(not_modified) --> 
	"Not Modified".
status_comment(not_found) -->
	"Not Found".
status_comment(forbidden) -->
	"Forbidden".
status_comment(authorise) -->
	"Authorization Required".
status_comment(server_error) -->
	"Internal Server Error".

authenticate(Method, '') --> !,
	"WWW-Authenticate: ",
	atom(Method).
authenticate(Method, Realm) -->
	authenticate(Method, ''),
	"Realm=\"", atom(Realm), "\"".

date(Time) -->
	"Date: ",
	(   { Time == now }
	->  now
	;   rfc_date(Time)
	),
	"\r\n".
	
modified(file(File)) --> !,
	{ time_file(File, Time)
	}, 
	modified(Time).
modified(Time) -->
	"Last-modified: ",
	(   { Time == now }
	->  now
	;   rfc_date(Time)
	),
	"\r\n".

content_length(ascii_string(String)) --> !,
	{ length(String, Len)
	},
	content_length(Len).
content_length(file(File)) --> !,
	{ size_file(File, Len)
	},
	content_length(Len).
content_length(html(Tokens)) --> !,
	{ html_print_length(Tokens, Len)
	},
	content_length(Len).
content_length(Len) -->
	{ number_codes(Len, LenChars)
	},
	"Content-Length: ", string(LenChars),
	"\r\n".

content_type(Main/Sub) --> !,
	"Content-Type: ",
	atom(Main),
	"/",
	atom(Sub),
	"\r\n".
content_type(Type) --> !,
	"Content-Type: ",
	atom(Type),
	"\r\n".

header_field(Name, Value) -->
	{ var(Name)
	}, !,
	field_name(Name),
	":",
	blanks,
	string(ValueChars),
	blanks_to_nl, !,
	{ field_to_prolog(Name, ValueChars, Value)
	}.
header_field(Name, Value) -->
	field_name(Name),
	": ",
	field_value(Value),
	"\r\n".

field_to_prolog(content_length, ValueChars, ContentLength) :- !,
	number_codes(ContentLength, ValueChars).
field_to_prolog(cookie, ValueChars, Cookies) :- !,
	debug(cookie, 'Cookie: ~s', [ValueChars]),
	phrase(cookies(Cookies), ValueChars).
field_to_prolog(set_cookie, ValueChars, SetCookie) :- !,
	debug(cookie, 'SetCookie: ~s', [ValueChars]),
	phrase(set_cookie(SetCookie), ValueChars).
field_to_prolog(_, ValueChars, Atom) :-
	atom_codes(Atom, ValueChars).

field_value(set_cookie(Name, Value, Options)) --> !,
	atom(Name), "=", atom(Value),
	set_cookie_options(Options).
field_value(Atomic) -->
	atom(Atomic).

set_cookie_options([]) -->
	[].
set_cookie_options([secure=true|T]) --> !,
	" ; secure",
	set_cookie_options(T).
set_cookie_options([Name=Value|T]) -->
	" ; ", field_name(Name), "=",
	atom(Value),
	set_cookie_options(T).


%	Process a sequence of [Name(Value), ...] attributes for the
%	header.

header_fields([]) -->
	[].
header_fields([H|T]) -->
	{ H =.. [Name, Value]
	},
	field_name(Name),
	": ",
	field_value(Value),
	"\r\n",
	header_fields(T).

%	field_name(?PrologName)
%
%	Convert between prolog_name and HttpName

field_name(Name) -->
	{ var(Name)
	}, !,
	rd_field_chars(0':, Chars),
	{ atom_codes(Name, Chars)
	}.
field_name(mime_version) --> !,
	"MIME-Version".
field_name(Name) -->
	{ atom_codes(Name, Chars)
	},
	wr_field_chars(Chars).

rd_field_chars(End, [C0|T]) -->
	[C],
	{ C \== End, !,
	  (   C == 0'-
	  ->  C0 = 0'_
	  ;   code_type(C, to_upper(C0))
	  )
	},
	rd_field_chars(End, T).
rd_field_chars(_, []) -->
	[].

wr_field_chars([C|T]) -->
	[C2], !,
	{ to_lower(C2, C)
	},
	wr_field_chars2(T).
wr_field_chars([]) -->
	[].

wr_field_chars2([0'_|T]) --> !,
	"-",
	wr_field_chars(T).
wr_field_chars2([C|T]) --> !,
	[C],
	wr_field_chars2(T).
wr_field_chars2([]) -->
	[].

%	now
%	rfc_date(+Time)

now -->
	{ get_time(Time)
	},
	rfc_date(Time).

rfc_date(Time) -->
	{ convert_time(Time, CDate)
	},
	sub(CDate, 1-3),
	", ",
	sub(CDate, 5-7),
	sub(CDate, 21-4),
	sub(CDate, 11-9).

sub(String, From-Len) -->
	{ substring(String, From, Len, S),
	  string_to_list(S, Chars)
	},
	string(Chars).
	

		 /*******************************
		 *	   REQUEST DCG		*
		 *******************************/

request(Fd, [method(Method)|Header]) -->
	method(Method),
	blanks,
	nonblanks(Query),
	{ http_location(Parts, Query),
	  append(Parts, Header0, Header)
	},
	request_header(Fd, Header0), !.
request(Fd, [unknown(What)|Header]) -->
	string(What),
	eos, !,
	{   http_read_header(Fd, Header)
        ->  true
	;   Header = ""
	}.

method(get) -->
	"GET", !.
method(post) -->
	"PUT", !.
method(post) -->
	"POST", !.

request_header(_, []) -->		% Old-style non-version header
	blanks,
	eos, !.
request_header(Fd, [http_version(Version)|Header]) -->
	http_version(Version),
	blanks,
	eos, !,
	{   Version = 1-_
	->  http_read_header(Fd, Header)
	;   Header = []
	}.

http_version(Version) -->
	blanks,
	"HTTP/",
	http_version_number(Version).

http_version_number(Major-Minor) -->
	integer(Major),
	".",
	integer(Minor).


		 /*******************************
		 *	      COOKIES		*
		 *******************************/

%	cookies are of the format NAME=Value; ...

cookies([Name=Value|T]) -->
	blanks,
	cookie(Name, Value), !,
	blanks,
	(   ";"
	->  cookies(T)
	;   { T = [] }
	).

cookie(Name, Value) -->
	cookie_name(Name),
	"=",
	cookie_value(Value).

cookie_name(Name) -->
	{ var(Name)
	}, !,
	rd_field_chars(0'=, Chars),
	{ atom_codes(Name, Chars)
	}.

cookie_value(Value) -->
	chars_to_semicolon_or_blank(Chars),
	{ atom_codes(Value, Chars)
	}.

chars_to_semicolon_or_blank([]) -->
	peek(0';), !.
chars_to_semicolon_or_blank([]) -->
	blank, !.
chars_to_semicolon_or_blank([H|T]) -->
	[H], !,
	chars_to_semicolon_or_blank(T).
chars_to_semicolon_or_blank([]) -->
	[].

peek(C, L, L) :-
	L = [C|_].

set_cookie(set_cookie(Name, Value, Options)) -->
	blanks,
	cookie(Name, Value),
	cookie_options(Options).

cookie_options([H|T]) -->
	blanks,
	";",
	blanks,
	cookie_option(H), !,
	cookie_options(T).
cookie_options([]) -->
	blanks.


cookie_option(secure=true) -->
	"secure", !.
cookie_option(Name=Value) -->
	rd_field_chars(0'=, NameChars),
	"=", blanks,
	chars_to_semicolon(ValueChars),
	{ atom_codes(Name, NameChars),
	  atom_codes(Value, ValueChars)
	}.

chars_to_semicolon([]) -->
	blanks,
	peek(0';), !.
chars_to_semicolon([H|T]) -->
	[H], !,
	chars_to_semicolon(T).
chars_to_semicolon([]) -->
	[].


		 /*******************************
		 *	     REPLY DCG		*
		 *******************************/

%	Typical reply:
%
%	HTTP/1.1 200 OK

reply(Fd, [http_version(HttpVersion), status(Status, Comment)|Header]) -->
	http_version(HttpVersion),
	blanks,
	(   status_number(Status)
	->  []
	;   integer(Status)
	),
	string(Comment),
	blanks_to_nl,
	blanks,
	{ http_read_header(Fd, Header)
	}.


		 /*******************************
		 *	      READ HEADER	*
		 *******************************/

%	http_read_header(+Fd, -Header)
%
%	Read Name: Value lines from FD until an empty line is encountered.
%	Field-name are converted to Prolog conventions (all lower, _ instead
%	of -): Content-Type: text/html --> content_type(text/html)

http_read_header(Fd, Header) :-
	read_header_data(Fd, Text),
	parse_header(Text, Header).

read_header_data(Fd, Header) :-
	read_line_to_codes(Fd, Header, Tail),
	read_header_data(Header, Fd, Tail),
	debug(header, 'Header = ~n~s~n', [Header]).

read_header_data("\r\n", _, _) :- !.
read_header_data("\n", _, _) :- !.
read_header_data("", _, _) :- !.
read_header_data(_, Fd, Tail) :-
	read_line_to_codes(Fd, Tail, NewTail),
	read_header_data(Tail, Fd, NewTail).

parse_header(Text, Header) :-
	phrase(header(Header), Text),
	debug(header, 'Fields: ~w~n', [Header]).


header([Att|T]) -->
	header_field(Name, Value), !,
	{ Att =.. [Name, Value]
	},
	blanks,
	header(T).
header([]) -->
	blanks,
	[].
