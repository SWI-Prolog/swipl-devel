:- module(my_httpd,
	  [ go/1
	  ]).
:- use_module(library(pce)).
:- use_module(library('http/httpd')).
:- use_module(library('http/html_write')).
:- use_module(library('draw/importpl')).

%	Create server at Port

go(Port) :-
	new(_, my_httpd(Port)).

:- pce_begin_class(my_httpd, httpd, "Demo Web server").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->request is sent after the super-class  has received a complete request
header. We get the `path' and  have   a  Prolog predicate generating the
replies.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

request(HTTPD, Request:sheet) :->
	"A request came in."::
	get(Request, path, Path),
	reply(Path, HTTPD).

:- discontiguous
	reply/2.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->reply_html takes <Module>:<DCGRuleSet> to formulate a reply. This uses
the  html_write  library,  converting  a  complex  Prolog  term  into  a
formatted HTML document. The complex  term   can  invoke  additional DCG
rulesets, providing nicely structured content-generation.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

reply('/', HTTPD) :- !,
	send(HTTPD, reply_html, my_httpd:frames).

frames -->
	html(html([ head(title('Demo')),
		    frameset([cols('25%,75%')],
			     [ frame([ src('/index'),
				       name(index)
				     ]),
			       frame([ src('/blank'),
				       name(body)
				     ])
			     ])
		  ])).


reply('/blank', HTTPD) :-
	send(HTTPD, reply_html, my_httpd:blank).

blank -->
	page(title('Blank'),
	     []).
	       
reply('/index', HTTPD) :-
	send(HTTPD, reply_html, my_httpd:index).

index -->
	page(title('Index'),
	     [ a([ href('/text'), target(body) ],
		 [ 'Show text' ]),
	       br([]),
	       a([ href('/picture'), target(body) ],
		 [ 'Show picture' ])
	     ]).

reply('/text', HTTPD) :-
	send(HTTPD, reply_html, my_httpd:text).
	       
text -->
	page(title('Text'),
	     [ p(['Just showing a little text'])
	     ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reply a graphical object. The server translates   the graphical to a GIF
or JPEG bitmap and provides the proper   HTTP reply header. You can also
embed graphicals into the HTML structures used above.

The drawing itself is exported from the  demo program PceDraw and turned
into an XPCE graphical using the support library draw/importpl.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

reply('/picture', HTTPD) :-
	make_picture(Gr),
	send(HTTPD, reply, Gr, 'image/gif').
	       
make_picture(Dev) :-
	new(Dev, device),
	drawing(xpcenetscape, Drawing),
	realise_drawing(Dev, Drawing).

%	Drawing imported from PceDraw

drawing(xpcenetscape,
	[ compound(new(A, figure),
		   drawing([ display(box(137, 74)+radius(17),
				     point(0, 0)),
			     display(text('XPCE', center, normal),
				     point(52, 30))
			   ]),
		   point(163, 183)),
	  compound(new(B, figure),
		   drawing([ display(box(137, 74)+radius(17),
				     point(0, 0)),
			     display(text('Netscape', center, normal),
				     point(42, 30))
			   ]),
		   point(350, 183)),
	  connect(connection(A,
			     B,
			     handle(w, h/2, link, east),
			     handle(0, h/2, link, west)) +
		    arrows(both))
	]).

:- pce_end_class(my_httpd).

