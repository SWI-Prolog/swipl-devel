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
:- use_module(html_hierarchy).
:- use_module(html_refman).

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

reply(/, @nil, HTTPD) :-
	send(HTTPD, reply_html, pce_http_man:frames).

frames -->
	html(html([ head(title('XPCE web-manual')),
		    frameset([cols('20%,80%')],
			     [ frame([ src('/classhierarchy'),
				       name(hierarchy)
				     ]),
			       frame([ src('/welcome'),
				       name(description)
				     ])
			     ])
		  ])).

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
		     | \classrows(Classes)
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
	html(tr([ td(\class_name(Name)),
		  td(Summary)
		])).

class_name(Class) -->
	{ object(Class), !,
	  get(Class, name, Name)
	},
	class_name(Name).
class_name(Name) -->
	{ www_form_encode(Name, Encoded),
	  atom_concat('/class?name=', Encoded, URL)
	},
	html(a([href(URL), target(description)], Name)).

classlist([], _) -->
	[].
classlist([H], _) -->
	class_name(H).
classlist([H|T], S) -->
	html([\class_name(H), S]),
	classlist(T, S).

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
	       p([]),
	       \description(Class),
	       p([]),
	       \classtable(Class)
	     ]).

description(Obj) -->
	{ html_description(Obj, String),
	  get(String, value, HTML)
	},
	[HTML].
	

classtable(Class) -->
	html_begin(table(border(2),
			 align(center),
			 width('100%'))),
	summary_row(Class),
	meta_class_row(Class),
	super_class_row(Class),
	subclasses_row(Class),
	instance_variables(Class),
	html_end(table).

summary_row(Class) -->
	{ get(Class, summary, Summary),
	  Summary \== @nil
	}, !,
	head_row('Summary', Summary).
summary_row(_) -->
	[].

meta_class_row(Class) -->
	{ get(Class, class, MetaClass),
	  \+ get(MetaClass, name, class)
	}, !,
	head_row('Meta Class', \class_name(MetaClass)).
meta_class_row(_) -->
	[].

super_class_row(Class) -->
	{ get(Class, super_class, Super),
	  Super \== @nil
	}, !,
	head_row('Super Class', \class_name(Super)).
super_class_row(_) -->
	[].

subclasses_row(Class) -->
	{ get(Class, sub_classes, Subs),
	  Subs \== @nil,
	  chain_list(Subs, List)
	}, !,
	head_row('Sub Classes', \classlist(List, ', ')).
subclasses_row(_) -->
	[].

head_row(Name, Value) -->
	html(tr([ th([align(right), colspan(2)], Name),
		  td(Value)
		])).


instance_variables(Class) -->
	{ get(Class, instance_variables, Vector),
	  object(Vector, Term),
	  Term =.. [_|Vars]
	},
	html([ tr([ th([ colspan(3),
			 bgcolor(yellow)
		       ],
		       'Instance variables')
		  ]),
	       tr([ th('Name'),
		    th('Type'),
		    th('Summary')
		  ])
	     | \variables(Vars)
	     ]).

variables([]) -->
	[].
variables([H|T]) -->
	{ get(H, name, Name),
	  get(H, type, Type),
	  get(H, summary, Summary)
	},
	html([ tr([td(Name), td(\type(Type)), td(Summary)])
	     ]),
	variables(T).

%	/classhierarchy
%	/classhierarchy?root=name
%
%	Emit the class hierarchy

reply('/classhierarchy', Form, HTTPD) :-
	(   Form \== @nil,
	    get(Form, value, root, Root)
	->  true
	;   Root = object
	),
	get(HTTPD, request, Request),
	(   get(Request, value, 'Cookie', Cookie)
	;   Cookie = []
	),
	send(HTTPD, reply_html, pce_http_man:classhierarchy(Root, Cookie)).
reply(Path, @nil, HTTPD) :-
	html_hierarchy_image(Path, Image), !,
	send(HTTPD, reply, Image).

classhierarchy(Root, Cookie) -->
	page(title('XPCE Class Hierarchy'),
	     [ h3('Class Hierarchy')
	     | \html_hierarchy(Root, gen_subclass, class_name, Cookie)
	     ]).

gen_subclass(Super, Sub) :-
	get(@pce, convert, Super, class, Class),
	get(Class, sub_classes, Chain),
	chain_list(Chain, List),
	member(SubClass, List),
	get(SubClass, name, Sub).

%	/man?for=Spec
%
%	As manpce/1: handle @ref, class, class->method, class<-method, etc.

reply('/man', Form, HTTPD) :-
	get(Form, for, String),
	atom_to_method(String, Object),
	send(HTTPD, reply_html, pce_http_man:objpage(Object)).
	
objpage(Object) -->
	page([title('XPCE Manual')],
	     [\objdoc(Object)
	     ]).

objdoc(Object) -->
	{ send(Object, instance_of, class)
	}, !,
	classdoc(Object).
objdoc(SendMethod) -->
	{ send(SendMethod, instance_of, behaviour)
	}, !,
	html(dl(\behaviour(SendMethod))).
objdoc(Object) -->
	html(dl(\object(Object))).


behaviour(M) -->
	html([ dt(\headline(M)),
	       dd(\description(M))
	     ]).

object(@Ref) -->
	html([ dt(b([@, Ref])),
	       dd(\description(@Ref))
	     ]).

headline(SM) -->
	{ send(SM, instance_of, send_method)
	}, !,
	html([ \contextclass(SM),
	       '->',
	       b(\name(SM)),
	       '(',
	       \argv(SM),
	       ')'
	     ]).
headline(GM) -->
	{ send(GM, instance_of, get_method),
	  get(GM, return_type, Return)
	}, !,
	html([ \contextclass(GM),
	       '->',
	       b(\name(GM)),
	       '(',
	       \argv(GM),
	       ') --> ',
	       \type(Return)
	     ]).
headline(V) -->
	{ send(V, instance_of, variable),
	  get(V, access_arrow, Arrow),
	  get(V, type, Type)
	}, !,
	html([ \contextclass(V),
	       Arrow,
	       b(\name(V)),
	       ': ',
	       \type(Type)
	     ]).
	       
contextclass(SM) -->
	{ get(SM, context, Class),
	  send(Class, instance_of, class)
	},
	class_name(Class).

name(Obj) -->
	{ send(Obj, has_get_method, name), !,
	  get(Obj, name, Name)
	},
	html(Name).
name(Obj) -->
	html(Obj).

argv(Method) -->
	{ get(Method, types, Argv),
	  get(Argv, size, Size)
	},
	argv(1, Size, Argv).
	
argv(I, AC, V) -->
	{ I =< AC, !,
	  get(V, element, I, Type)
	},
	(   { get(Type, argument_name, ArgName),
	      ArgName \== @nil
	    }
	->  html([ var(ArgName),
		   =,
		   \type(Type)
		 ])
	;   type(Type)
	),
	(   { I < AC
	    }
	->  [ ', ' ]
	;   []
	),
	{ NI is I + 1
	},
	argv(NI, AC, V).
argv(_, _, _) -->
	[].




%	/welcome
%
%	Temporary stuff

reply('/welcome', @nil, HTTPD) :-
	send(HTTPD, reply_html, pce_http_man:welcome).

welcome -->
	page([ title('Welcome')
	     ],
	     [ form([ action('/man'),
		      method('GET')
		    ],
		    [ input([name(for)]),
		      input([type(submit)])
		    ])
	     ]).

%	/blank
%
%	Just emit a blank page

reply('/blank', @nil, HTTPD) :-
	send(HTTPD, reply_html, pce_http_man:blankpage).

blankpage -->
	page([title(blank)], []).

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
