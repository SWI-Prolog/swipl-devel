/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(html_hierarchy,
	  [ html_hierarchy/6,		% +Root, :GenChild, :GenLabel
	    html_hierarchy_image/2,	% +Path, -Image
	    pageYOffset/2		% +Cookie, -Y
	  ]).
:- use_module(library(pce)).
:- use_module(html_write).

:- style_check(-atom).


:- meta_predicate
	html_hierarchy(+, :, :, +, -, +).

html_hierarchy(Root, GenChild, GenLabel, Cookie) -->
	{ strip_module(GenChild, M1, T1),
	  strip_module(GenLabel, M2, T2)
%	  ,format('Cookie = ~w~n', [Cookie])
	},
	script,
	hierarchy(Root, M1:T1, M2:T2, Cookie, 0, []).

hierarchy(Root, GenChild, GenLabel, Cookie, 0, _) --> !,
	{ findall(Child, gen_child(GenChild, Root, Child), Subs)
	},
	html([ \gen_label(GenLabel, Root),
	       br([])
	     | \subclasses(Subs, GenChild, GenLabel, Cookie, 1, [])
	     ]).
hierarchy(Root, GenChild, GenLabel, Cookie, Level, Lines) -->
	{ findall(Child, gen_child(GenChild, Root, Child), Subs),
	  (   Subs == []
	  ->  Pre = n
	  ;   (   expanded(Root, Cookie)
	      ->  Pre = m,
		  java_collapse(Root, ExpCol)
	      ;	  Pre = p,
		  java_expand(Root, ExpCol)
	      )
	  ),
	  concat_atom([Pre, Level|Lines], :, Place),
	  concat_atom(['/images/hierarchy/', Place], ImgSrc),
	  SubLevel is Level + 1
	},
	(   {Pre==n}
	->  html([ img([ src(ImgSrc), alt(''), align(top) ], []),
		   \gen_label(GenLabel, Root),
		   br([])
		 ])
	;   {Pre==m}
	->  html([ a([href(ExpCol)],
		     img([ src(ImgSrc),
			   alt(''),
			   align(top),
			   border(0)
			 ])),
		   \gen_label(GenLabel, Root),
		   br([])
		 | \subclasses(Subs, GenChild, GenLabel, Cookie, SubLevel, Lines)
		 ])
	;   html([ a([href(ExpCol)],
		     img([ src(ImgSrc),
			   alt(''),
			   align(top),
			   border(0)
			 ])),
		   \gen_label(GenLabel, Root),
		   br([])
		 ])
	).
	

script -->				% tagged window.location.pathname
	html(script(
'function pageY()
{ if ( navigator.appName == "Microsoft Internet Explorer" ) 
    return document.body.scrollTop;
  else
    return window.pageYOffset;
}

function collapse(name)
{ var x = document.cookie.split("#");
  var a = x[0].split("&");
  var r = new String("&");
  var y = pageY();

  for(var i=0; i < a.length; i++)
  { if ( a[i] != name && a[i] != "" )
    { r += a[i] + "&";
    }
  }

  r += "#" + y + " ;path=" + window.location.pathname;
  document.cookie = r;
  window.location.reload();
}

function expand(name)
{ var x = document.cookie.split("#");
  var y = pageY();

  if ( x[0] == "" )
  { x[0] = "&" + name + "&";
  } else
  { x[0] += name + "&";
  }
  document.cookie = x[0] + "#" + y + " ;path=" + window.location.pathname;

  window.location.reload();
//window.onLoad = "scrollTo(0," + y + ")";
}

function expandall()
{ document.cookie = "all;path=" + windows.location.pathname;
  window.location.reload();
}
')).


%	pageYOffset(+Cookie, -Y)
%
%	Return the current page Y-offset from the cookie.  This value may
%	be used in the onLoad handler of the page-body.

pageYOffset(Cookie, Y) :-
	new(Re, regex('#\\([0-9]+\\)')),
	send(Re, search, Cookie),
	get(Re, register_value, Cookie, 1, int, Y).
pageYOffset(_, 0).


expanded(_, all) :- !.
expanded(Class, Cookie) :-
	www_form_encode(Class, Encoded),
	concat_atom([&, Encoded, &], Pattern),
	sub_atom(Cookie, _, _, _, Pattern), !.

java_expand(Class, Code) :-
	www_form_encode(Class, Encoded),
	sformat(Code, 'javascript:expand(\'~w\')', Encoded).
java_collapse(Class, Code) :-
	www_form_encode(Class, Encoded),
	sformat(Code, 'javascript:collapse(\'~w\')', Encoded).


subclasses([], _, _, _, _, _) -->
	[].
subclasses([H], GenChild, GenLabel, Cookie, Level, Lines) --> !,
	hierarchy(H, GenChild, GenLabel, Cookie, Level, Lines).
subclasses([H|T], GenChild, GenLabel, Cookie, Level, Lines) -->
	hierarchy(H, GenChild, GenLabel, Cookie, Level, [Level|Lines]),
	subclasses(T, GenChild, GenLabel, Cookie, Level, Lines).		       


		 /*******************************
		 *	    GENERATORS		*
		 *******************************/

gen_child(GenChildren, Root, Child) :-
	call(GenChildren, Root, Child).

gen_label(G, Class, A, B) :-
	call(G, Class, A, B).
	

		 /*******************************
		 *	      IMAGES		*
		 *******************************/

html_hierarchy_image(Path, Img) :-
	atom_concat('/images/hierarchy/', IName, Path), !,
	term_to_atom(Type:X, IName),
	(   X = N:VLines
	->  true
	;   N = X,
	    VLines = []
	),
	Left is (N-1)*20 + 10,
	H = 18,
	H2 is H//2,
	new(P, path(points := chain(point(Left, 0),
				    point(Left, H2),
				    point(N*20, H2)))),
	new(Img, pixmap(@nil, width := N*20, height := H)),
	vlines(VLines, Img, H),
	send(Img, draw_in, P),
	(   Type == m
	->  get(class(tree), class_variable, expanded_image, CV),
	    get(CV, value, ExpImg),
	    send(Img, draw_in, bitmap(ExpImg), point(Left-4, 5))
	;   Type == p
	->  get(class(tree), class_variable, collapsed_image, CV),
	    get(CV, value, ExpImg),
	    send(Img, draw_in, bitmap(ExpImg), point(Left-4, 5))
	;   true
	).

vlines([], _, _) :- !.
vlines(N:T, Img, H) :- !,
	X is (N-1)*20+10,
	send(Img, draw_in, line(X, 0, X, H)),
	vlines(T, Img, H).
vlines(N, Img, H) :-
	X is (N-1)*20+10,
	send(Img, draw_in, line(X, 0, X, H)).	
	
level(A0-B, Level, [B|A]) :- !,
	level(A0, Level, A).
level(B, B, []).






