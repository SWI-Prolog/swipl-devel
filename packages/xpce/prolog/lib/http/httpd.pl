/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(httpd,
	  [ html_to_buffer/2		% +Term, -Buffer
	  ]).
:- use_module(library(pce)).
:- use_module(html_write).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module implements a very  simple   HTTP  deamon skeleton, mainly to
allow a browser to contact the running system.

The typical way to use this module is   by  refining the class httpd and
then  redefining  ->request.  Request   typically    uses   ->reply   or
->reply_html to send a reply.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(httpd, socket, "The HTTP deamon socket").

variable(request, sheet*, get, "Currently processing request").

initialise(S, Port:[int]) :->
	default(Port, 0, ThePort),	% anonymous
	send_super(S, initialise, ThePort),
	send(S, record_separator, '\n\r?\n\r?'),
	send(S, input_message, message(@receiver, input, @arg1)),
	send(S, accept_message, message(@arg1, accepted)),
	send(S, listen, reuse := @on).

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
		     ?(@arg1, register_value, Header, 1, name)?downcase,
		     ?(@arg1, register_value, Header, 2, name)),
	     StartOfAtt),
	send(S, break_path, H),
	send(S, break_authorization, H),
	send(S, slot, request, H),
	(   send(@pce, debugging_subject, httpd)
	->  send(S, pp_request)
	;   true
	),
	(   send(S, builtin_request, H)
	;   send(S, request, H)
	;   send(S, server_error)
	;   true
	),
	(   object(S)
	->  send(S, slot, request, @nil)
	;   true
	).


break_path(_, Header:sheet) :->
	"Break form information from a path (GET)"::
	get(Header, path, Path),
	(   get(Header, request, 'GET'),
	    sub_atom(Path, B, _, A, ?)
	->  sub_atom(Path, 0, B, _, NewPath),
	    sub_atom(Path, _, A, 0, FormString),
	    send(Header, value, form, new(Form, sheet)),
	    decode_form_string(FormString, Form)
	;   send(Header, value, form, @nil),
	    NewPath = Path
	),
	www_form_encode(ThePath, NewPath),
	send(Header, path, ThePath).

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

%	Athorisation information
%
%	Authorization: Basic <Base64 for user:password>
%	Adds the entries user and password to the header

break_authorization(_, Header:sheet) :->
	(   get(Header, value, authorization, Auth),
	    get(Auth, split, chain('Basic', Base64)),
	    get(Base64, base64_decode, Decoded),
	    get(Decoded, split, :, chain(User, Password))
	->  send(Header, value, user, User),
	    send(Header, value, password, Password)
	;   true
	).

:- pce_group(request).

builtin_request(S, Header:sheet) :->
	"Handle a built-in request '/http/object?reference='"::
	get(Header, path, '/httpd/object'),
	get(Header, form, Form), Form \== @nil,
	get(Form, reference, ObjRef),
	get(@pce, object_from_reference, ObjRef, Obj),
	send(S, reply, Obj).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->request

Virtual method. To use  this  class,   always  subclass  it and redefine
->request. The reference implementation  here  is   used  to  test basic
communication and responses.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

request(S, Header:sheet) :->
	"Process a request.  The argument is the header"::
	(   get(Header, path, '/no')
	->  send(S, forbidden, '/no')
	;   get(Header, path, '/maybe')
	->  (   get(Header, value, user, jan),
	        get(Header, value, password, test)
	    ->	send(S, reply, 'You hacked me')
	    ;	send(S, authorization_required)
	    )
	;   send(S, reply, 'Nice try')
	).

:- pce_group(reply).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->reply

Send a reply to the  client.  This  method   is  used  to  reply using a
complete object. If HTML generation is required   it may be desirable to
use ->reply_html, which uses library(html_write) to   produce  HTML in a
user-friendly way.

Typically, the data for reply is  a   string,  a  text_buffer, a file, a
resource or an image. Images are encoded  in jpeg and sent as image/jpeg
mime-type or as GIF of the provided   mime-type is image/gif. Other data
is sent as text/plain unless specified.

Messages with status other than "200 OK"  are normally send using one of
the specialised methods from the *error* group (see below).

Finally, optional extra header information may be send. Please check the
HTTP1.1 documentation at

	http://www.w3.org/Protocols/rfc2616/rfc2616.html

Some useful extra header fields:

	Cache-Control:	no-cache	% Do not cache this data
	Expires: <date>			% Set expiration-date
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

reply(S,
      Reply:data='string|source_sink|pixmap',
      Type:type=[name],
      Status:status=[name],
      Header:header=[sheet]) :->
	"Send back a reply"::
	(   send(Reply, instance_of, source_sink)
	->  get(Reply, contents, Data)
	;   send(Reply, instance_of, image)
	->  new(TB, text_buffer),
	    send(TB, undo_buffer_size, 0),
	    default(Type, 'image/jpeg', TheType),
	    atom_concat('image/', ImgType, TheType),
	    send(Reply, save, TB, ImgType),
	    get(TB, contents, Data),
	    free(TB)
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
		    format('Content-Length: %d\n', Data?size),
		    format('Content-Type: %s\n', TheType)
		  ]),
	(   get(S, request, Request),
	    Request \== @nil,
	    (	get(Request, value, connection, 'Keep-Alive')
	    ;	get(Request, value, http_version, '1.1')
	    )
	->  Connection = 'Keep-Alive'
	;   Connection = close
	),
	send(S, format, 'Connection: %s\n', Connection),
	(   Header \== @default
	->  send(Header, for_all,
		 message(S, format, '%s: %s\n', @arg1?name, @arg1?value))
	;   true
	),
	send(S, format, '\n'),
	send(S, append, Data),
	(   Connection == 'Keep-Alive'
	->  true
	;   send(S, free)
	).

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

authorization_required(S, Method:'method=[{Basic}]', Realm:[name]) :->
	"Report a 401 autorization required"::
	default(Method, 'Basic', M),
	default(Realm, 'ByPassword', R),
	concat_atom([M, ' realm="', R, '"'], AuthValue),
	new(Sheet, sheet),
	send(Sheet, value, 'WWW-Authenticate', AuthValue),
	send(S, reply_html, authorization_required,
	     '401 Authorization Required',
	     Sheet).

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

authorization_required -->
	page([ title('401 Authorization Required')
	     ],
	     [ h1('Authorization Required'),
	       p(['This server could not verify that you ',
		  'are authorized to access the document ',
		  'requested.  Either you supplied the wrong ',
		  'credentials (e.g., bad password), or your ',
		  'browser doesn''t understand how to supply ',
		  'the credentials required.'
		 ])
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
%	programming.  Currently this deals with:
%
%		# char_array
%		Emit the text (quoted)
%
%		# device with (some) graphicals providing <-href
%		Image with client-side image-map
%
%		# Other graphicals and images
%		An image
%
%		# Anything else
%		<-print_name (quoted)


:- multifile
	html_write:expand/3.

html_write:expand(@Ref) -->
	{ (   object(@Ref)
	  ;   catch(get(@Ref, self, _), _, fail) % force creation
	  )
	},
	expand_object(@Ref), !.
	
expand_object(Object) -->
	{ send(Object, instance_of, char_array), !,
	  get(Object, value, Name)
	},
	html_quoted(Name).
expand_object(Object) -->
	{ send(Object, instance_of, device),
	  image_map(Object, MapId, Map), !,
	  gensym(map, MapId),
	  atom_concat('#', MapId, MapRef),
	  object_url(Object, Data)
	},
	html([ img([ src(Data),
		     border(0),
		     usemap(MapRef)
		   ]),
	       Map
	     ]).
expand_object(Object) -->
	{ (   send(Object, instance_of, graphical)
	  ;   send(Object, instance_of, image)
	  ), !,
	  object_url(Object, Data)
	},
	html(img(src(Data))).
expand_object(Object) -->
	{ get(Object?print_name, value, Name)
	},
	html_quoted(Name).

%	object_url(+Object, -URL)
%
%	Make sure the object has a named reference and is locked against
%	the garbage collector and then return a URL of the form
%
%		'/httpd/object?reference=<Ref>'
%
%	Locking and using named references is required as the object will
%	only be requested later and we need a relyable existence check to
%	avoid security problems due to crashes.  Possibly we should make
%	a table to allow for cleanup as well as disallow requesting any
%	object with a known name.  See also ->builtin_request

object_url(Object, URL) :-
	get(Object, object_reference, Ref),
	integer(Ref), !,
	send(Object, lock_object, @on),
	gensym('obj_', Name),
	send(Object, name_reference, Name),
	atom_concat('/httpd/object?reference=', Name, URL).
object_url(Object, URL) :-
	get(Object, object_reference, Ref),
	atom_concat('/httpd/object?reference=', Ref, URL).


image_map(Dev, Id, map(name(Id), Map)) :-
	send(Dev, instance_of, device), !,
	send(Dev, compute),		% need proper layout
	new(MapGrs, chain),
	\+ get(Dev, find, @default,
	       and(message(@arg1, has_get_method, href),
		   @arg1 \== Dev,
		   message(MapGrs, append, @arg1),
		   new(or)),
	       _),
	chain_list(MapGrs, List),
	List \== [],
	make_map(List, Dev, Map).

make_map([], _, []).
make_map([Gr|T0], Dev, [area([href(Href)|Area])|T]) :-
	get(Gr, href, Href),
	area(Gr, Dev, Area),
	make_map(T0, Dev, T).

%	area(+Graphical, +Device -MapAreaAttributes)
%
%	Incomplete mapping of attributes.  Should also deal with circles
%	and try to map other items as reasonable as possible to paths.
%	This one will then be the fall-back.
%
%	The complication arrives from the fact we should give the bounding
%	box of the graphical relative to the bounding box of the device and
%	neither is very willing to help much.

area(Gr, Dev, [shape(rect), coords(Coords)]) :-
	get(Dev, offset, point(OX, OY)),
	get(Dev, area, area(DX, DY, _, _)),
	get(Gr, absolute_position, Dev, point(AX, AY)),
	get(Gr, position, point(PX, PY)),
	get(Gr, area, area(X,Y,W,H)),
	GrX is X + (AX-PX) + (DX-OX),
	GrY is Y + (AY-PY) + (DY-OY),
	GrR is GrX + W,
	GrB is GrY + H,
	concat_atom([GrX, GrY, GrR, GrB], ',', Coords).
	
	
