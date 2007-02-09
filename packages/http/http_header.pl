/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

	    http_timestamp/2,		% +Time, -HTTP string

	    http_post_data/3,		% +Stream, +Data, +HdrExtra

	    http_read_header/2,		% +Fd, -Header
	    http_join_headers/3,	% +Default, +InHdr, -OutHdr
	    http_update_encoding/3	% +HeaderIn, -Encoding, -HeaderOut
	  ]).
:- use_module(library(readutil)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(memfile)).
:- use_module(dcg_basics).
:- use_module(html_write).
:- use_module(mimetype).
:- use_module(mimepack).


		 /*******************************
		 *	    READ REQUEST	*
		 *******************************/

%%	http_read_request(+FdIn, -Request)
%
%	Read an HTTP request-header from FdIn and return the broken-down
%	request fields as +Name(+Value) pairs in a list.

http_read_request(In, [input(In)|Request]) :-
	read_line_to_codes(In, Codes),
	(   Codes == end_of_file
	->  debug(header, 'end-of-file', [])
	;   debug(header, 'First line: ~s~n', [Codes]),
	    phrase(request(In, Request), Codes)
	).


%%	http_read_reply_header(+FdIn, -Reply)
%
%	Read the HTTP reply header. Throws   an exception if the current
%	input does not contain a valid reply header.

http_read_reply_header(In, [input(In)|Reply]) :-
	read_line_to_codes(In, Codes),
	(   Codes == end_of_file
	->  debug(header, 'end-of-file', []),
	    throw(error(syntax(http_reply_header, end_of_file), _))
	;   debug(header, 'First line: ~s~n', [Codes]),
	    (   phrase(reply(In, Reply), Codes)
	    ->  true
	    ;   throw(error(syntax(http_reply_header, Codes), _))
	    )
	).


		 /*******************************
		 *	  FORMULATE REPLY	*
		 *******************************/

%%	http_reply(+Data, +Out:stream) is det.
%%	http_reply(+Data, +Out:stream, +HdrExtra) is det.
%
%	Data is one of
%	
%		* html(HTML)
%		HTML tokens as produced by html//1 from html_write.pl
%		
%		* file(+MimeType, +FileName)
%		Reply content of FileName using MimeType
%		
%		* tmp_file(+MimeType, +FileName)
%		Same as =file=, but do not include modification time
%		
%		* stream(+In, +Len)
%		Reply content of stream.
%		
%		* cgi_stream(+In, +Len)
%		Reply content of stream, which should start with an
%		HTTP header, followed by a blank line.  This is the
%		typical output from a CGI script.
%		
%	@tbd	Complete documentation	

http_reply(What, Out) :-
	http_reply(What, Out, [connection(close)]).

http_reply(html(HTML), Out, HrdExtra) :- !,
	phrase(reply_header(html(HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(file(Type, File), Out, HrdExtra) :- !,
	phrase(reply_header(file(Type, File), HrdExtra), Header),
	format(Out, '~s', [Header]),
	open(File, read, In, [type(binary)]),
	call_cleanup(copy_stream_data(In, Out),
		     close(In)).
http_reply(tmp_file(Type, File), Out, HrdExtra) :- !,
	phrase(reply_header(tmp_file(Type, File), HrdExtra), Header),
	format(Out, '~s', [Header]),
	open(File, read, In, [type(binary)]),
	call_cleanup(copy_stream_data(In, Out),
		     close(In)).
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
http_reply(moved_temporary(To), Out, HrdExtra) :- !,
	phrase(page([ title('302 Moved Temporary')
		    ],
		    [ h1('Moved Temporary'),
		      p(['The document is currently ',
			 a(href(To), ' Here')
			]),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(moved_temporary(To, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).
http_reply(see_other(To),Out,HdrExtra) :- !,
       phrase(page([ title('303 See Other')
                    ],
                    [ h1('See Other'),
                      p(['See other document ',
                         a(href(To), ' Here')
                        ]),
                      address(httpd)
                    ]),
               HTML),
        phrase(reply_header(see_other(To, HTML), HdrExtra), Header),
        format(Out, '~s', [Header]),
        print_html(Out, HTML).
http_reply(not_found(URL), Out, HrdExtra) :- !,
	phrase(page([ title('404 Not Found')
		    ],
		    [ h1('Not Found'),
		      p(['The requested URL ', tt(URL),
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
	'$messages':translate_message(ErrorTerm, Lines, []),
	phrase(page([ title('500 Internal server error')
		    ],
		    [ h1('Internal server error'),
		      p(\html_message_lines(Lines)),
		      address(httpd)
		    ]),
	       HTML),
	phrase(reply_header(status(server_error, HTML), HrdExtra), Header),
	format(Out, '~s', [Header]),
	print_html(Out, HTML).


html_message_lines([]) -->
	[].
html_message_lines([nl|T]) --> !,
	html([br([])]),
	html_message_lines(T).
html_message_lines([flush]) -->
	[].
html_message_lines([Fmt-Args|T]) --> !,
	{ format(string(S), Fmt, Args)
	},
	html([S]),
	html_message_lines(T).
html_message_lines([Fmt|T]) --> !,
	{ format(string(S), Fmt, [])
	},
	html([S]),
	html_message_lines(T).

%%	http_join_headers(+Default, +Header, -Out)
%	
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


%%	http_update_encoding(+HeaderIn, -Encoding, -HeaderOut)
%	
%	Allow for rewrite of the  header,   adjusting  the  encoding. We
%	distinguish three options. If  the   user  announces  `text', we
%	always use UTF-8 encoding. If   the user announces charset=utf-8
%	we  use  UTF-8  and  otherwise  we  use  octet  (raw)  encoding.
%	Alternatively we could dynamically choose for ASCII, ISO-Latin-1
%	or UTF-8.

http_update_encoding(Header0, utf8, [content_type(Type)|Header]) :-
	select(content_type(Type0), Header0, Header),
	sub_atom(Type0, 0, _, _, 'text/'), !,
	(   sub_atom(Type0, S, _, _, ';')
	->  sub_atom(Type0, 0, B, _, S)
	;   B = Type0
	),
	atom_concat(B, '; charset=UTF-8', Type).
http_update_encoding(Header, utf8, Header) :-
	memberchk(content_type(Type), Header),
	(   sub_atom(Type, _, _, _, 'UTF-8')
	;   sub_atom(Type, _, _, _, 'utf-8')
	), !.
http_update_encoding(Header, octet, Header).


%%	content_length_in_encoding(+Encoding, +In, -Bytes)
%	
%	Determine hom much bytes are required to represent the data from
%	stream In using the given encoding.  Fails if the data cannot be
%	represented with the given encoding.

content_length_in_encoding(Enc, Stream, Bytes) :-
	open_null_stream(Out),
	set_stream(Out, encoding(Enc)),
	stream_property(Stream, position(Here)),
	(   catch((copy_stream_data(Stream, Out),
		   flush_output(Out)), _, fail)
	->  byte_count(Out, Bytes0)
	;   true
	),
	close(Out),
	set_stream_position(Stream, Here),
	(   var(Bytes0)
	->  fail
	;   Bytes = Bytes0
	).


		 /*******************************
		 *	    POST SUPPORT	*
		 *******************************/

%%	http_post_data(+Data, +Out:stream, +HdrExtra) is det.
%
%	@tbd	Complete docs

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
	open(File, read, In, [type(binary)]),
	call_cleanup(copy_stream_data(In, Out),
		     close(In)).
http_post_data(cgi_stream(In, _Len), Out, HdrExtra) :- !,
	debug(obsolete, 'Obsolete 2nd argument in cgi_stream(In,Len)', []),
	http_post_data(cgi_stream(In), Out, HdrExtra).
http_post_data(cgi_stream(In), Out, HdrExtra) :- !,
	http_read_header(In, Header0),
	http_update_encoding(Header0, Encoding, Header),
	content_length_in_encoding(Encoding, In, Size),
	http_join_headers(HdrExtra, Header, Hdr2),
	phrase(reply_header(cgi_data(Size), Hdr2), HeaderText),
	format(Out, '~s', [HeaderText]),
	set_stream(Out, encoding(Encoding)),
	call_cleanup(copy_stream_data(In, Out),
		     set_stream(Out, encoding(octet))).
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
	format(string(ContentType), 'multipart/form-data; boundary=~w', [Boundary]),
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
	format(string(ContentType), 'multipart/mixed; boundary=~w', [Boundary]),
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
reply_header(moved_temporary(To, Tokens), HdrExtra) -->
	vstatus(moved_temporary),
	date(now),
	header_field('Location', To),
	header_fields(HdrExtra),
	content_length(html(Tokens)),
	content_type(text/html),
	"\r\n".
reply_header(see_other(To,Tokens),HdrExtra) -->
      vstatus(see_other),
      date(now),
      header_field('Location',To),
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

status_number(continue)	       --> "100".
status_number(ok)	       --> "200".
status_number(moved)	       --> "301".
status_number(moved_temporary) --> "302".
status_number(see_other)       --> "303".
status_number(not_modified)    --> "304". 
status_number(not_found)       --> "404".
status_number(forbidden)       --> "403".
status_number(authorise)       --> "401".
status_number(server_error)    --> "500".

status_comment(continue) -->
	"Continue".
status_comment(ok) -->
	"OK".
status_comment(moved) -->
	"Moved Permanently".
status_comment(see_other) -->
      "See Other".
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
	whites,
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
field_to_prolog(host, ValueChars, Host) :- !,
	(   append(HostChars, [0':|PortChars], ValueChars),
	    catch(number_codes(Port, PortChars), _, fail)
	->  atom_codes(HostName, HostChars),
	    Host = HostName:Port
	;   atom_codes(Host, ValueChars)
	).
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

%%	field_name(?PrologName)
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
%%	rfc_date(+Time)

now -->
	{ get_time(Time)
	},
	rfc_date(Time).

%%	rfc_date(+Time)// is det.
%	
%	Write time according to RFC1123 specification as required by the
%	RFC2616 HTTP protocol specs. 

rfc_date(Time, String, Tail) :-
	stamp_date_time(Time, Date, 'UTC'),
	format_time(codes(String, Tail), '%a, %d %b %Y %H:%M:%S GMT', Date).
	
%%	http_timestamp(+Time, -Atom)
%	
%	Generate a description of a Time in HTTP format (RFC1123)

http_timestamp(Time, Atom) :-
	stamp_date_time(Time, Date, 'UTC'),
	format_time(atom(Atom), '%a, %d %b %Y %H:%M:%S GMT', Date).


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
method(put) -->
	"PUT", !.
method(head) -->
	"HEAD", !.
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

%%	http_read_header(+Fd, -Header)
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

header(List) -->
	header_field(Name, Value), !,
	{ mkfield(Name, Value, List, Tail)
	},
	blanks,
	header(Tail).
header([]) -->
	blanks,
	[].

mkfield(host, Host:Port, [host(Host),port(Port)|Tail], Tail) :- !.
mkfield(Name, Value, [Att|Tail], Tail) :-
	Att =.. [Name, Value].
