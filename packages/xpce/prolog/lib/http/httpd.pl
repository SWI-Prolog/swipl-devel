/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(httpd, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module implements a very  simple   HTTP  deamon skeleton, mainly to
allow a browser to contact the running system.

The typical way to use this module is   by  refining the class httpd and
then  redefining  ->request.  Request   typically    uses   ->reply   or
->reply_html to issue a reply.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(httpd, socket, "The HTTP deamon socket").

variable(request, sheet*, get, "Currently processing request").

initialise(S, Port:[int]) :->
	default(Port, 0, ThePort),	% anonymous
	send_super(S, initialise, ThePort),
	send(S, record_separator, '\n\r?\n\r?'),
	send(S, input_message, message(@receiver, input, @arg1)),
	send(S, accept_message, message(@arg1, accepted)),
	send(S, listen).

:- pce_global(@http_header_regex,
	      new(regex('^\\([^:]+\\):\\s *\\(.*\\)$'))).
:- pce_global(@http_path_regex,
	      new(regex('\\(\\w+\\)\\s *\\(\\S +\\)\\s *HTTP/\\([0-9.]+\\)\r?\n?'))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Process the request into a  sheet   holding  the request attributes. The
first line is translated  into  the   attributes  `request',  `path' and
`http_version'.

If the action is a GET   and path contains form-attributes, ->break_path
breaks the path into the  plain  path   and  form-data  in the attribute
`form'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_group(connection).

accepted(S) :->
	"A new connection is established on this socket"::
	(   send(@pce, debugging_subject, httpd),
	    get(S, peer_name, Peer)
	->  (   send(Peer, instance_of, tuple)
	    ->	send(@pce, format, 'New connection from %s:%s\n', 
		     Peer?first, Peer?second)
	    ;	send(@pce, format, 'New connection from %s\n', Peer)
	    )
	;   true
	).

:- pce_group(request).

input(S, Header:string) :->
	"Process input.  The argument is the header"::
	(   send(@pce, debugging_subject, httpd)
	->  send(@pce, write, Header)
	;   true
	),
	new(H, sheet),
	send(@http_path_regex, match, Header),
	get(@http_path_regex, register_end, StartOfAtt),
	get(@http_path_regex, register_value, Header, 1, name, Request),
	get(@http_path_regex, register_value, Header, 2, name, Path),
	get(@http_path_regex, register_value, Header, 3, name, HttpVersion),
	send(H, value, request, Request),
	send(H, value, path, Path),
	send(H, value, http_version, HttpVersion),
	send(@http_header_regex, for_all, Header,
	     message(H, value,
		     ?(@arg1, register_value, Header, 1, name),
		     ?(@arg1, register_value, Header, 2, name)),
	     StartOfAtt),
	send(S, break_path, H),
	send(S, slot, request, H),
	(   send(@pce, debugging_subject, httpd)
	->  send(S, pp_request)
	;   true
	),
	(   send(S, request, H)
	->  send(S, slot, request, @nil)
	;   send(S, server_error),
	    send(S, slot, request, @nil)
	).

break_path(_, Header:sheet) :->
	"Break form information from a path (GET)"::
	get(Header, path, Path),
	(   get(Header, request, 'GET'),
	    sub_atom(Path, B, _, A, ?)
	->  sub_atom(Path, 0, B, _, NewPath),
	    sub_atom(Path, _, A, 0, FormString),
	    send(Header, path, NewPath),
	    send(Header, value, form, new(Form, sheet)),
	    decode_form_string(FormString, Form)
	;   send(Header, value, form, @nil)
	).

decode_form_string('', _) :- !.
decode_form_string(S, Sheet) :-
	sub_atom(S, B, _, A, &), !,
	sub_atom(S, 0, B, _, A1),
	sub_atom(S, _, A, 0, Rest),
	decode_form_string(A1, Sheet),
	decode_form_string(Rest, Sheet).
decode_form_string(S, Sheet) :-
	sub_atom(S, B, _, A, =), !,
	sub_atom(S, 0, B, _, UrlName),
	sub_atom(S, _, A, 0, UrlValue),
	www_form_encode(Name, UrlName),
	www_form_encode(Value, UrlValue),
	send(Sheet, value, Name, Value).

:- pce_group(virtual).

request(S, Header:sheet) :->
	"Process a request.  The argument is the header"::
	(   get(Header, path, '/no')
	->  send(S, forbidden, '/no')
	;   send(S, reply, 'Nice try')
	).

:- pce_group(reply).

reply(S,
      Reply:data='string|source_sink|image',
      Type:type=[name],
      Status:status=[name],
      Header:header=[sheet]) :->
	"Send a reply back"::
	(   send(Reply, instance_of, source_sink)
	->  get(Reply, contents, Data)
	;   send(Reply, instance_of, image)
	->  new(TB, text_buffer),
	    send(TB, undo_buffer_size, 0),
	    send(Reply, save, TB, jpeg),
	    get(TB, contents, Data),
	    free(TB),
	    TheType = 'image/jpeg'
	;   Data = Reply
	),
	(   var(TheType)
	->  default(Type, 'text/plain', TheType)
	;   true
	),
	get(new(date), rfc_string, Now),
	(   send(Reply, instance_of, file)
	->  get(Reply, time, modified, ModifiedDate),
	    get(ModifiedDate, rfc_string, Modified)
	;   Modified = Now
	),
	default(Status, '200 OK', TheStatus),
	send_list(S,
		  [ format('HTTP/1.1 %s\n', TheStatus),
		    format('Date: %s\n', Now),
		    format('Last-modified: %s\n', Modified),
		    format('Connection: Keep-Alive\n'),
		    format('Content-Length: %d\n', Data?size),
		    format('Content-Type: %s\n', TheType)
		  ]),
	(   Header \== @default
	->  send(Header, for_all,
		 message(S, format, '%s: %s\n', @arg1?name, @arg1?value))
	;   true
	),
	send(S, format, '\n'),
	send(S, append, Data).

reply_html(S, Term:prolog, Status:status=[name], Header:header=[sheet]) :->
	"Reply HTML from a structured Prolog term"::
	(   html_to_buffer(Term, TB)
	->  send(S, reply, TB, 'text/html', Status, Header)
	;   send(S, server_error, 'Translation to HTML failed')
	).

:- pce_group(error).

forbidden(S, What:[name]) :->
	"Report a 403 permission error"::
	(   What == @default
	->  get(S?request, path, Path)
	;   Path = What
	),
	send(S, reply_html, forbidden(Path), '403 Forbidden').

not_found(S, What:[char_array]) :->
	"Report a 404 not found error"::
	(   What == @default
	->  get(S?request, path, Path)
	;   Path = What
	),
	send(S, reply_html, not_found(Path), '404 Not Found').

moved(S, Where:char_array) :->
	"Report a 301 Moved Permanently"::
	send(S, reply_html, moved(Where),
	     '301 Moved Permanently',
	     sheet(attribute('Location', Where))).

server_error(S, What:[char_array]) :->
	"Report a 500 server error"::
	send(S, reply_html, server_error(What), '500 Internal Server Error').

:- pce_group(debug).

pp_request(S) :->
	"Print the current request"::
	get(S, request, Sheet),
	send(Sheet, for_all,
	     message(@pce, format, '%s: %s\n', @arg1?name, @arg1?value)),
	(   get(Sheet, value, form, Form),
	    Form \== @nil
	->  send(@pce, format, '\nForm data:\n'),
	    send(Form, for_all,
		 message(@pce, format, '\t%s: %s\n', @arg1?name, @arg1?value))
	;   true
	).
	    
:- pce_end_class(httpd).


		 /*******************************
		 *	    ERROR PAGES		*
		 *******************************/

forbidden(URL) -->
	page([ title('403 Forbidden')
	     ],
	     [ h1('Forbidden'),
	       p(['You don''t have permission to access ', URL,
		  ' on this server'
		 ]),
	       address(httpd)
	     ]).

not_found(URL) -->
	page([ title('404 Not found')
	     ],
	     [ h1('Not found'),
	       p(['The requested URL ', URL,
		  ' was not found on this server'
		 ]),
	       address(httpd)
	     ]).

server_error(@default) --> !,
	page([ title('500 Internal Server Error')
	     ],
	     [ h1('Server error'),
	       p(['The server failed to process your request'
		 ]),
	       address(httpd)
	     ]).
server_error(Message) -->
	page([ title('500 Internal Server Error')
	     ],
	     [ h1('Server error'),
	       p(['The server failed to process your request'
		 ]),
	       p(Message),
	       address(httpd)
	     ]).

moved(Where) -->
	page([ title('301 Moved Permanently')
	     ],
	     [ h1('Moved Permanently'),
	       p([ 'The document has moved ', a(href(Where), ' Here')
		 ]),
	       address(httpd)
	     ]).


		 /*******************************
		 *	  HTML GENERATION	*
		 *******************************/

%	html_to_buffer(+Term, -TB)
%
%	Convert an HTML term (as defined in html.pl) into an HTML formatted
%	text-buffer.  We use a text-buffer for fast appending and the fact
%	that it is applicable to ->reply.

html_to_buffer(Term, TB) :-
	phrase(Term, Tokens),
	new(TB, text_buffer),
	send(TB, undo_buffer_size, 0),	% we don't need undo
	pce_open(TB, write, Fd),
	print_html(Fd, Tokens),
	close(Fd).

%	html_dcg:expand(Object)
%
%	Allow placing certain XPCE objects in the token list to minimise
%	programming.  We will extend this with images, etc.

:- multifile
	html_write:expand/3.

html_write:expand(Object) -->
	{ object(Object), !,
	  (   send(Object, instance_of, char_array)
	  ->  get(Object, value, Name)
	  ;   get(Object, print_name, Name)
	  )
	},
	html_quoted(Name).

:- use_module(html_write).
