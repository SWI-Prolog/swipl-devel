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

function getCookie(name)
{ var cookie = " " + document.cookie;
  var search = " " + name + "=";
  var setStr = null;
  var offset = 0;
  var end = 0;
  if (cookie.length > 0)
  { offset = cookie.indexOf(search);
    if (offset != -1)
    { offset += search.length;
      end = cookie.indexOf(";", offset)
      if (end == -1)
      { end = cookie.length;
      }
      setStr = cookie.substring(offset, end);
    }
  }
  return(setStr);
}

function setCookie(name, value)
{ document.cookie = name + "=" + value;
}

function expand(name)
{ var e = getCookie("expand");

  if ( e )
  { e += name + "&";
  } else
  { e = "&" + name + "&";
  }

  setCookie("expand", e);
  setCookie("y", pageY());
  window.location.reload();
}

function collapse(name)
{ var e = getCookie("expand");

  if ( e )
  { var a = e.split("&");
    var r = new String("&");
    
    for(var i=0; i < a.length; i++)
    { if ( a[i] != name && a[i] != "" )
      { r += a[i] + "&";
      }
    }

    setCookie("expand", r);
  }

  setCookie("y", pageY());
  window.location.reload();
}

function expandall()
{ setCookie("expand", "all");
  setCookie("y", pageY());
  window.location.reload();
}
')).


%	pageYOffset(+Cookie, -Y)
%
%	Return the current page Y-offset from the cookie.  This value may
%	be used in the onLoad handler of the page-body.

pageYOffset(Cookie, Y) :-
	new(Re, regex('y=\\([0-9]+\\)')),
	send(Re, search, Cookie),
	get(Re, register_value, Cookie, 1, int, Y).
pageYOffset(_, 0).


:- dynamic
	ccode/2.

class_code(Class, Code) :-
	ccode(Class, Code), !.
class_code(Class, Code) :-
	flag(ccode, Code, Code+1),
	assert(ccode(Class, Code)).


expanded(_, Cookie) :-
	send(regex('expand=all'), search, Cookie), !.
expanded(Class, Cookie) :-
	class_code(Class, Code),
	concat_atom([&, Code, &], Pattern),
	sub_atom(Cookie, _, _, _, Pattern), !.

java_expand(Class, Code) :-
	class_code(Class, CCode),
	sformat(Code, 'javascript:expand(\'~w\')', CCode).
java_collapse(Class, Code) :-
	class_code(Class, CCode),
	sformat(Code, 'javascript:collapse(\'~w\')', CCode).


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






