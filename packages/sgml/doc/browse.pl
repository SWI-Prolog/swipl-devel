/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(sgml_browse,
	  [ x/0,
	    browse/1
	  ]).
:- use_module(library('doc/xml_browse')).
:- use_module(doc(emit)).
:- use_module(xml_convert).

:- multifile
	doc:emit/3,
	doc:action/3.
:- dynamic
	doc:emit/3.

:- asserta((doc:emit([element(Tag, Attributes, Content)|T], PB, Mode) :-
	phrase(element(Tag, Attributes, Content), T1, T),
	emit(T1, PB, Mode))).

:- discontiguous
	element/5.

element(pldoc, _, Content) -->		% PLDOC
	{ setof(S, summary(Content, S), Summary)
	},
	Content,
	[ element(h1, [], ['Summary of predicates']),
	  element(table, [border=1, align=left], Summary)
	].
	
section(s1, h1).
section(s2, h2).
section(s3, h3).
section(s4, h4).

element(S, _, Content) -->
	{ section(S, H), !,
	  Content = [ element(title, _, Title),
		      element(_,     _, Body)
		    ]
	},
	[ element(H, [], Title)
	| Body
	].

	 
element(body1, [], Content) -->
	Content.
element(body2, [], Content) -->
	Content.
element(body3, [], Content) -->
	Content.
element(body4, [], Content) -->
	Content.

element(quote, A, Content) -->
	[ element(blockquote, A, Content)
	].
element(item, [],			% <ITEM>
	[ element(tag, [], Tag),
	  element(desc, [], Desc)
	]) -->
	[ element(li, [],
		  [ element(i, [], Tag),
		    element(br,[],[])
		  | Desc
		  ])
	].
element(fn, _, Content) -->
	[ \footnote(Content)
	].
element(code, A, Content) -->
	[ element(p, [], []),
	  element(pre, A, Content)
	].
element(name, [], Content) -->
	Content.
element(version, [], Content) -->
	Content.
element(arity, [], Content) -->
	Content.
element(jargon, A, Content) -->
	[ element(i, A, Content) ].
element(ref, _, Content) -->
	[ element(b, [], Content) ].
element(pref, _, Content) -->
	[ element(b, [], Content) ].
element(sh, _, Content) -->
	[ element(tt, [], Content) ].
element(aref, _, Content) -->
	[ element(i, [], Content) ].
element(verb, _, Content) -->
	[ element(tt, [], Content) ].
element(xmp, A, Content) -->
	{ memberchk(placement=block, A)
	}, !,
	[ \par,
	  \group([ \space(preserve)
		 | Content
		 ]),
	  @br
	].
element(xmp, _, Content) -->
	[ element(tt, [], Content) ].
element(c, _, Content) -->
	[ element(u, [], [element(tt, [], Content)])
	].
element(elem, _, Content) -->
	[ element(b, [], [element(tt, [], Content)])
	].
element(term, _, Content) -->		% TBD
	Content.
element(pref, [],
	[ element(name, _, [Name]),
	  element(arity, _, [Arity])
	]) -->
	[ element(b, [], [Name, /, Arity])
	].

element(head, _, _) -->
	[].

		 /*******************************
		 *	       TITLE		*
		 *******************************/

element(titlepage, _, Content) -->
	{ xml_select(Content, element(title), Title),
	  xml_select(Content, element(author), Author),
	  xml_select(Content, element(abstract), element(_, _, Abstract)),
	  Title = element(_, _, TheTitle)
	},
	[ Title,
	  element(center,
		  [],
		  [ element(h1, [], TheTitle),
		    Author
		  ]),
	  element(blockquote, [], Abstract)
	].
	  
element(author, _, Content) -->
	{ xml_select(Content, element(name), Name),
	  xml_select(Content, element(address), element(_, _, Address))
	},
	[ element(address, [],
		  [ Name,
		    element(br, [], [])
		  | Address
		  ])
	].

		 /*******************************
		 *	    DEFINITIONS		*
		 *******************************/

element(definitions, _, Content) -->
	[ element(dl, [], Content)
	].
element(definition, [], [Title, _Summary, element(desc, _, Content)]) -->
	[ element(dt, [], [Title]),
	  element(dd, [], Content)
	].
element(definition, [], [Title, element(desc, _, Content)]) -->
	[ element(dt, [], [Title]),
	  element(dd, [], Content)
	].
element(pred, [],			% <PRED>
	[ element(name, _, Name),
	  element(arglist, _, Args)
	]) -->
	{ insert_commas(Args, Args1)
	},
	Name,
	[ '(' ],
	Args1,
	[ ')' ].
element(arg, [], Content) -->
	[ element(i, [], Content)
	].
element(var, [], Content) -->
	[ element(i, [], Content)
	].

element(argdef, [], Content) -->	% <ARGDEF>
	[ element(dl, [], Content)
	].
element(valdef, [], Content) -->	% <ARGDEF>
	[ element(dl, [], Content)
	].
element(argval, [], [Title, element(desc, _, Content)]) -->
	[ element(dt, [], [Title]),
	  element(dd, [], Content)
	].
element(termitem, [], Content) -->
	element(pred, [], Content).
element(termitem, [], Content) -->
	Content.

insert_commas([], []).
insert_commas([Last], [Last]) :- !.
insert_commas([H|T0], [H, ', '|T]) :-
	insert_commas(T0, T).


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

x :-
	load_sgml_file('sgml2pl.sgml', Term),
	B = @b,
	free(B),
	browse(B, Term).

browse(Term) :-
	browse(_, Term).
browse(B, Term) :-
	send(new(B, xml_browser(Term)), open).


		 /*******************************
		 *	      SUMMARY		*
		 *******************************/

summary(Content, element(summary_entry, [], [Pred, Summary])) :-
	xml_select(Content, element(definition), Def),
	xml_select(Def,     element(/pred),      Pred),
	xml_select(Def,     element(/summary),   Summary).

element(summary_entry, _, [Pred, Summary]) -->
	[ element(tr, [], [ element(td, [], [Pred]),
			    element(td, [], [Summary])
			  ])
	].
element(summary, _, Content) -->
	Content.
		 
		 /*******************************
		 *	     HIERARCHY		*
		 *******************************/

:- multifile
	doc:caption/2.

doc:caption(element(S, _, Content), Title) :-
	section(S, _),
	Content = [element(title, _, [Title])|_].

	    
		 /*******************************
		 *	     AUTO-START		*
		 *******************************/

:- x.
