/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(pce_http_man,
	  [ pce_http_man/1		% ?Port
	  ]).

:- use_module(httpd).
:- use_module(html_write).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Demo for the XPCE HTTP Deamon. This  demo   simply  lists a table of all
classes and allows  for  browsing   through  the  instance-variables  of
classes. Maybe one day this will be expanded to a full manual server. 

To test it, do:

	?- [http_man].
	?- pce_http_man(8080).		% or some other port

Then run your browser and start at the url http://localhost:8080/
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


pce_http_man(Port) :-			% start on anonymous port
	var(Port), !,
	new(HTTPD, manpce_httpd),
	get(HTTPD, port, Port).
pce_http_man(Port) :-
	new(_, manpce_httpd(Port)).

:- pce_begin_class(manpce_httpd, httpd,
		   "Demo HTTPD for XPCE manual").

request(HTTPD, Request:sheet) :->
	get(Request, path, Path),
	get(Request, form, Form),
	reply(Path, Form, HTTPD).

:- pce_end_class.

:- discontiguous
	reply/3.

%	/
%
%	For now redirect to the class index

reply(/, @nil, HTTPD) :-
	send(HTTPD, moved, '/class/').

%	/class/
%
%	Provide a table with all classes

reply('/class/', @nil, HTTPD) :-
	new(Classes, chain),
	send(@classes, for_all, message(Classes, append, @arg1)),
	send(Classes, sort),
	chain_list(Classes, Sorted),
	send(HTTPD, reply_html, pce_http_man:classindex(Sorted)).

classindex(Classes) -->
	page(title('XPCE Class Index'),
	     [ h1([align(center)], 'XPCE Class Index'),
	       table([ border(2),
		       align(center)
		     ],
		     [ tr([ th('Name'),
			    th('Summary')
			  ])
		     | pce_http_man:classrows(Classes)
		     ])
	     ]).

classrows([]) -->
	[].
classrows([H|T]) -->
	classrow(H),
	classrows(T).

classrow(Name) -->
	{ get(@pce, convert, Name, class, Class),
	  get(Class, summary, Summary)
	},
	html(tr([ td(pce_http_man:class_name(Name)),
		  td(Summary)
		])).

class_name(Name) -->
	{ www_form_encode(Name, Encoded),
	  atom_concat('/class?name=', Encoded, URL)
	},
	html(a([href(URL)], Name)).

%	/class?name=classname
%
%	Provide documentation on the class.

reply('/class', Form, HTTPD) :-
	get(Form, name, Class), !,
	get(@pce, convert, Class, class, ClassObj),
	send(HTTPD, reply_html, pce_http_man:classdoc(ClassObj)).

classdoc(Class) -->
	{ get(Class, name, Name)
	},
	page([ title(['XPCE class ', Name])
	     ],
	     [ h1([align(center)], ['XPCE class ', em(Name)]),
	       pce_http_man:instance_variables(Class)
	     ]).

instance_variables(Class) -->
	{ get(Class, name, Name),
	  get(Class, instance_variables, Vector),
	  object(Vector, Term),
	  Term =.. [_|Vars]
	},
	html([ h2(['Instance variables for ', em(Name)]),
	       table([ border(2),
		       align(center)
		     ],
		     [ tr([ th('Name'),
			    th('Type'),
			    th('Summary')
			  ])
		     | pce_http_man:variables(Vars)
		     ])
	     ]).

variables([]) -->
	[].
variables([H|T]) -->
	{ get(H, name, Name),
	  get(H, type, Type),
	  get(H, summary, Summary)
	},
	html([ tr([td(Name), td(pce_http_man:type(Type)), td(Summary)])
	     ]),
	variables(T).

%	Catch all.  Do not add clauses for reply below this line

reply(Path, @nil, HTTPD) :-
	send(HTTPD, not_found, Path).



		 /*******************************
		 *             TYPES		*
		 *******************************/

type(T) -->
	{ get(T, kind, class), !,
	  get(T, context, C),
	  (   atom(C)
	  ->  Name = C
	  ;   get(C, name, Name)
	  )
	},
	type_default(T, '['),
	class_name(Name),
	type_supers(T),
	type_default(T, ']'),
	type_nil(T),
	type_vector(T).
type(T) -->
	{ get(T, name, Name),
	  atom_length(Name, L)
	},
	(   { L > 20,
	      make_breakable(Name, Full)
	    }
	->  html(Full)
	;   html(Name)
	).

make_breakable(Atom, Breakable) :-
	new(Breakable, string('%s', Atom)),
	send(regex('\\([,|]\\)'), for_all, Breakable,
		   message(@arg1, replace, @arg2, '\\1 ')).

type_default(T, C) -->
	{ get(T, supers, Supers), Supers \== @nil,
	  send(Supers, member, type(default))
	}, !,
	[ C ].
type_default(_, _) -->
	[].

type_nil(T) -->
	{ get(T, supers, Supers), Supers \== @nil,
	  send(Supers, member, type(nil))
	}, !,
	[ '*' ].
type_nil(_) -->
	[].

type_vector(T) -->
	{ get(T, vector, @on)
	}, !,
	[ '...' ].
type_vector(_) -->
	[].
       
type_supers(T) -->
	{ get(T, supers, Supers), Supers \== @nil,
	  chain_list(Supers, L)
	}, !,
	t_supers(L).
type_supers(_) -->
	[].

t_supers([]) -->
	[].
t_supers([H|T]) -->
	(   { get(H, name, nil)
	    ; get(H, name, default)
	    }
	->  []
	;   ['|'],
	    type(H)
	),
	t_supers(T).
