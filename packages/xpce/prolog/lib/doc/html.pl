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

:- module(html,
	  [
	  ]).
:- use_module(doc(emit)).
:- use_module(library(pce)).
:- use_module(doc(util)).
:- use_module(doc(url_fetch)).
:- use_module(doc(form)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module connects the xml2pl library   to the XPCE document rendering
primitives, providing parsing HTML files as  well as required hooks into
emit/3 to deal with the HTML xml2pl element/3 terms.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	     RENDERING		*
		 *******************************/

:- multifile
	doc:emit/3,
	doc:action/3.

doc:emit(document(_, Content), PB, Mode) :- !,
	emit(Content, PB, Mode).
doc:emit([element(p, A, C)|T0], PB, Mode) :- !,
	join_paragraphs(element(p, A, C), T0, P, T),
	phrase(P, T1, T),
	emit(T1, PB, Mode).
doc:emit([element(Tag, Attributes, Content)|T], PB, Mode) :-
	(   phrase(element(Tag, Attributes, Content, Mode), T1, T)
	;   phrase(element(Tag, Attributes, Content), T1, T)
	), !,
	emit(T1, PB, Mode).

%	Delete empty paragraphs before real paragraphs.

join_paragraphs(element(p, [], []), [element(p, A, C)|T0], P, T) :- !,
	join_paragraphs(element(p, A, C), T0, P, T).
join_paragraphs(P, T, P, T).


		 /*******************************
		 *	      MACROS		*
		 *******************************/

:- multifile
	element/5,			% +Tag, +Attributes, +Content
	element/6.			% +Tag, +Attributes, +Content, +ParBox

%	Basic text handling

element(p, [], Content) -->		% <P>
	[ \par,
	  \ignorespaces
	| Content
	].
element(p, Options, Content) -->	% <P Options>
	[ \parskip,
	  \parbox([\ignorespaces|Content],
		  [ width(0),
		    rubber(@quote_rubber)
		  | Options
		  ]),
	  @br
	].
element(div, A, C) -->			% <DIV>
	element(p, A, C).		% TBD: Should force clearance of
					% shape-graphicals!
element(br, _, _) -->			% <BR>
	[ @br,
	  \ignorespaces
	].

%	Rules

element(hr, _, _) -->			% <HR>
	{ new(L, line),
	  new(B, grbox(L, bottom, @hfill_rubber))
	},
	[ @br, B, @br
	].

% 	Basic font management

element(b, _, Content) -->		% <B>
	[ \group([\setfont(weight, bold)|Content])
	].
element(i, _, Content) -->		% <I>
	[ \group([\setfont(slant, i)|Content])
	].
element(u, _, Content) -->		% <U>
	[ \group([\ul|Content])
	].
element(tt, _, Content) -->		% <TT>
	[ \group([\setfont(fixed,  true)|Content])
	].
element(small, _, Content) -->		% <SMALL>
	[ \group([\setfont(font_size, -1)|Content])
	].
element(big, _, Content) -->		% <BIG>
	[ \group([\setfont(font_size, +1)|Content])
	].
element(font, Attributes, Content) -->
	{ phrase(font_attributes(Attributes), Group, Content)
	},
	[ \group(Group)
	].

font_attributes([]) -->
	[].
font_attributes([H|T]) -->
	font_attribute(H), !,
	font_attributes(T).
font_attributes([H|T]) -->
	{ print_message(warning, html_ignored_attribute(font, H))
	},
	font_attributes(T).

font_attribute(size=Size) -->
	(   { integer(Size)
	    }
	->  [ \setfont(size, Size)
	    ]
	;   { atom_codes(Size, Codes),
	      catch(number_codes(Diff, Codes), _, fail)
	    }
	->  [ \setfont(font_size, Diff)
	    ]
	).
%font_attribute(face=Face) -->
%	[ \setfont(face, Face)
%	].
font_attribute(encoding=Enc) -->	% not really HTML, but handy anyway
	[ \setfont(encoding, Enc)
	].
font_attribute(colour=Colour) -->
	[ \colour(Colour)
	].
font_attribute(color=Colour) -->
	[ \colour(Colour)
	].


%	Sections

element(h1, Options, Title) -->		% <H1>
	header(1, Options, Title).
element(h2, Options, Title) -->		% <H2>
	header(2, Options, Title).
element(h3, Options, Title) -->		% <H3>
	header(3, Options, Title).
element(h4, Options, Title) -->		% <H4>
	header(4, Options, Title).


%	Informational font management

element(em,     A, Content) --> element(i,  A, Content).
element(var,    A, Content) --> element(i,  A, Content).
element(arg,    A, Content) --> element(i,  A, Content).
element(kbd,    A, Content) --> element(tt, A, Content).
element(code,   A, Content) --> element(tt, A, Content).
element(samp,   A, Content) --> element(tt, A, Content).
element(strong, A, Content) --> element(b,  A, Content).
element(cite,	A, Content) --> element(i,  A, Content).

%	Analysis support tags

element(abbrev,	  _, Content) --> Content.
element(acronym,  _, Content) --> Content.
element(au,	  _, Content) --> Content.
element(cite,	  _, Content) --> Content.
element(language, _, Content) --> Content.
element(person,	  _, Content) --> Content.
element(q,	  _, Content) --> Content.
	  
%	Lists

element(ul, _, Content) -->		% <UL>
	[ \list([ class(bullet_list)
		],
		Content)
	].
element(ol, _, Content) -->		% <OL>
	[ \list([ class(enum_list)
		],
		Content)
	].
element(li, _, Content) -->		% <LI>
	[ \li(Content)
	].
element(dl, _, Content) -->		% <DL>
	[ \list([ class(definition_list)
		],
		Content)
	].
element(dt, _, DefTitle) -->		% <DT>
	[ \dt(DefTitle)
	].
element(dd, _, DefDescription) -->	% <DD>
	[ \dd(DefDescription)
	].
element(menu, Attrs, Content) -->	% <MENU>
	element(ul, Attrs, Content).
element(dir, Attrs, Content) -->	% <DIR>
	element(ul, Attrs, Content).

%	Paragraph making environments

element(blockquote, _, Content) -->	% <BLOCKQUOTE>
	[ \parskip,
	  @quote_margin,
	  \parbox([ \ignorespaces|Content],
		  [ width(0),
		    rubber(@quote_rubber)
		  ]),
	  @quote_margin,
	  @br
	].
element(center, _, Content) -->		% <CENTER>
	[ \parskip,
	  \parbox([ \ignorespaces|Content],
		  [ width(0),
		    align(center),
		    rubber(@quote_rubber)
		  ]),
	  @br
	].
element(address, _, Content) -->	% <ADDRESS>
	[ \group([\setfont(slant, i)|Content])
	].

%	Images

element(img, Attributes, _, Mode) -->	% <IMG>
	{ option(src(Url), Attributes),
	  get(Mode, base_url, BaseUrl),
	  get_url_to_file(Url, BaseUrl, File),
	  new(B, bitmap),
	  (   send(B, load,  File)
	  ->  true
	  ;   send(B, image, @noimage)
	  ),
	  new(GrBox, grbox(B)),
	  apply_options(Attributes, image_option, GrBox)
	},
	(   { memberchk(align=middle, Attributes)
	    }
	->  [ @br, @hfill, GrBox, @hfill, @br
	    ]
	;   [ GrBox
	    ]
	).

image_option(src(_), _).
image_option(align(middle), GrBox) :- !,
	send(GrBox, alignment, center).
image_option(align(X), GrBox) :-
	send(GrBox, alignment, X).
image_option(border(X), GrBox) :-
	send(GrBox?graphical, pen, X).
image_option(alt(Alt), GrBox) :-
	atomic(Alt), !,
	send(GrBox?graphical, help_message, tag, Alt).

resource(noimage, image, image('16x16/pce.xpm')).

:- pce_global(@noimage, new(image(resource(noimage)))).

%	Buttons and anchors

element(a, Attr, Content) -->		% <A HREF=<URL>> ... </A>
	{ memberchk(href=Url, Attr),
	  new(Msg, message(@browser, goto_url, Url))
	}, !,
	(   { memberchk(name=Label, Attr) } % NAME=<Label>
	->  [ \anchor(Label,
		      [ \button(Msg, Content, Url)
		      ])
	    ]
	;   [ \button(Msg, Content, Url)
	    ]
	).
element(a, Attr, Content) -->		% <A NAME=<Label>> ... </A>
	{ memberchk(name=Label, Attr)
	}, !,
	[ \anchor(Label, Content)
	].

%	Tables

element(table, Attr, Content) -->	% <TABLE>
	[ \table(Attr, Content)
	].
element(tbody, Attr, Content) -->	% <TBODY>
	[ \tbody(Attr)
	| Content
	].
element(thead, Attr, Content) -->	% <THEAD>
	[ \thead(Attr, Content)
	].
element(tr, Attr, Content) -->		% <TR>
	[ \tr(Attr, Content) 
	].
element(td, Attr, Content) -->		% <TD>
	[ \td(Attr, Content) 
	].
element(th, Attr, Content) -->		% <TH>
	[ \td(Attr, Content) 
	].
element(col, Attr, _) -->		% <COL>
	[ \col(Attr)
	].

%	Preformatted output

element(pre, _, Content) -->		% <PRE>
	[ @br,
	  \group([ \setfont(fixed, @on),
		   \pre(Content)
		 ]),
	  @br
	].

%	Document structure

element(html, _, Content) -->		% <HTML>
	Content.
element(head, _, Content) -->		% <HEAD>
	Content.
element(title, _, Title) -->		% <TITLE>
	{ content_to_atom(Title, Label)
	},
	[ \title(Label)
	].
element(meta, _, _) -->			% <META>
	[].
element(link, _, _) -->			% <LINK>
	[].
element(body, Attributes, Content) -->	% <BODY>
	[ \body(Attributes)
	| Content
	].

element(base, Attrs, [], Mode) -->	% <BASE href=BaseURL>
	{ (   option(href(BaseURL), Attrs)
	  ->  send(Mode, base_url, BaseURL)
	  ;   true			% warn?
	  )
	},
	[].

		 /*******************************
		 *	      SCRIPTS		*
		 *******************************/

%	should allow for `language=prolog' and define something to do
%	with it!?

element(script, _, _) -->
	{ print_message(informational, html_ignored_script(script))
	},
	[].

		 /*******************************
		 *	       TITLES		*
		 *******************************/

header_attributes(1,
		  [ \setfont(font_size, 3), \setfont(weight, bold) ],
		  @h1_above, @h1_below).
header_attributes(2,
		  [ \setfont(font_size, 2), \setfont(weight, bold) ],
		  @h1_above, @h1_below).
header_attributes(3,
		  [ \setfont(font_size, 1), \setfont(weight, bold) ],
		  @h1_above, @h1_below).
header_attributes(4,
		  [ \setfont(weight, bold) ],
		  @h1_above, @h1_below).

header(Level, Options, Content) -->
	{ header_attributes(Level, FontAtt, Above, Below),
	  append(FontAtt, [\ignorespaces|Content], Title)
	},
	[ @br, Above, @br
	],
	(   { Options == [] }
	->  [ \group(Title)
	    ]
	;   [ \parbox(Title,
		      [ width(0),
			rubber(@quote_rubber)
		      | Options
		      ])
	    ]
	),
	[ @br, Below
	],
	element(p, [], []).		% Some tools omit the <P>
	
	
		 /*******************************
		 *	       FORMS		*
		 *******************************/

element(form, Attrs, Content) -->	% <FORM>
	[ \form(Attrs, Content)
	].
element(input, Attrs, _) -->		% <INPUT>
	{ option(type(Type), Attrs, text)
	},
	input_element(Type, Attrs).

input_element(submit, Attrs) -->
	{ new(B, html_submit_input(Attrs))
	},
	[ \input(B)
	].
input_element(text, Attrs) -->
	{ new(TI, html_text_input(Attrs))
	},
	[ \input(TI)
	].
input_element(hidden, Attrs) -->
	{ option(name(Name), Attrs),
	  option(value(Value), Attrs)
	},
	[ \input(hidden(Name, Value))
	].
	
element(select, Attrs, Content) -->	% <SELECT>
	{ option(name(Name), Attrs),
	  (   option(size(Size), Attrs),
	      get(@pce, convert, Size, int, IntSize),
	      IntSize > 1
	  ->  new(Select, html_select_browser(Name, Attrs, Content))
	  ;   new(Select, html_select_menu(Name, Attrs, Content))
	  )
	},
	[ \input(Select)
	].
	

		 /*******************************
		 *	      CATCH ALL		*
		 *******************************/

element(E, _, Content) -->
	{ print_message(warning, html_ignored_element(E))
	},
	Content.


		 /*******************************
		 *	ADDITIONAL ACTIONS	*
		 *******************************/

:- dynamic
	current_form/1.

%	forms

doc:action(form(Attrs, Content), PB, Mode) :-
	new(F, html_form(Attrs, PB)),
	asserta(current_form(F), Ref),	% global state!
	emit(Content, PB, Mode),
	erase(Ref).
doc:action(input(hidden(Name, Value)), _PB, _Mode) :-
	(   current_form(Form)
	->  send(Form, hidden_input, Name, Value)
	;   print_message(warning, doc(expected_context(input, form)))
	).
doc:action(input(Obj), PB, _Mode) :-
	send(PB, append, grbox(Obj, center)),
	(   current_form(Form)
	->  send(Form, append, Obj)
	;   print_message(warning, doc(expected_context(input, form)))
	).
	   

		 /*******************************
		 *	UTILITY PREDICATES	*
		 *******************************/

%	relative_width(+Spec, -Frac)
%
%	Convert <num>% to a float representing the percentage

relative_width(Spec, Frac) :-
	sub_atom(Spec, B, _, 0, '%'),
	sub_atom(Spec, 0, B, _, NumAtom),
	atom_chars(NumAtom, Chars),
	catch(number_chars(Num, Chars), _, fail),
	Frac is Num/100.

		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(html_ignored_attribute(Element, Attribute)) -->
	[ 'Failed to handle attribute ~p of element ~w'-[Attribute, Element] ].
prolog:message(html_ignored_element(Element)) -->
	[ 'Ignored element "~w", using content'-[Element] ].
prolog:message(html_ignored_script(_Element)) -->
	[ 'Ignored script' ].
