/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(doc2tex,
	  [ doc2tex/1,			% +SGMLFile
	    x/0,
	    v/0
	  ]).
:- use_module(xml_convert).

:- op(500, fx, #).
:- op(500, fx, ?).

user:portray(element(E,_,_)) :-
	format('<~w>', [E]).

doc2tex(In) :-
	absolute_file_name(In,
			   [ extensions([sgml]),
			     access(read)
			   ], InFile),
	file_name_extension(Base, _, InFile),
	file_name_extension(Base, tex, TeXFile),
	load_sgml_file(InFile, Term),
	del(_),
	telling(Old),
	tell(TeXFile),
	to_tex(Term, TeX),
	emit_tex(TeX),
	told,
	tell(Old).
	
x :-
	doc2tex(doc).

v :-
	shell('acroread x.pdf &').

to_tex([], []).
to_tex([element(E,A,C)|T], L) :-
	phrase(tex(E,A,C), L1, T), !,
	to_tex(L1, L).
to_tex([H0|T0], [H|T]) :-
	(   to_tex1(H0, H)
	->  true
	;   format(user_error, 'Error: skipped ~p~n', [H])
	),
	to_tex(T0, T).

to_tex1(\X, \X).
to_tex1(env(N,C), env(N,C)).
to_tex1({G}, {G}).
to_tex1(H, H) :-
	atomic(H).


:- dynamic
	stored/1.

set(Value) :-
	asserta(stored(Value)).
del(Value) :-
	retractall(stored(Value)).


:- discontiguous
	tex/5.

tex(pldoc, _, Content) -->
	[ \documentclass(?(#('11pt')), #article), \n,
	  \usepackage(#makeidx), \n,
	  \usepackage(#url), \n,
	  \usepackage(#a4wide), \n,
	  \usepackage(#longtable), \n,
	  \parskip('5pt'),
	  \makeindex,
	  \n,
	  \parindent('0pt'),
	  \sloppy, \n,

	  \begin(#document), \n
	| Content
	],
	summary(Content),
	[ \n(2),
	  \printindex, \n,
	  \end(#document), \n
	].

		 /*******************************
		 *	       HEAD		*
		 *******************************/

tex(head, _, Content) -->
	Content.
tex(version, _, Content) -->
	{ set(version(Content))
	}.
tex('release-date', _, Content) -->
	{ set(release_date(Content))
	}.
tex(package, _, Content) -->
	{ set(package(Content))
	}.

		 /*******************************
		 *	     TITLEPAGE		*
		 *******************************/

tex(titlepage, _, Content) -->
	{ xml_select(Content, element(title), element(_, _, Title)),
	  xml_select(Content, element(author/name), element(_, _, Name)),
	  xml_select(Content, element(author/address), element(_, _, Address)),
	  xml_select(Content, element(author/email), element(_, _, Email)),
	  xml_select(Content, element(abstract), element(_, _, Abstract)),

	  stored(version(Version)),
	  stored(release_date(Date))
	},
	[ \begin(#titlepage),

	  env(center,
	      [ {[\'Huge' | Title]}, \br('5pt'),
		{\'Large', \textit([ 'Version ',
				     \aml(Version),
				     ', ',
				     \aml(Date)
				   ])
		}, \br('20pt'),
		{\'Large', \textit(Name)}, \br('10pt'),
		\aml(Address), \br,
		{\bf, 'E-mail: '}, \url(Email)
	      ]),
		
	  env(abstract, [\noindent | Abstract]),
	  \end(#titlepage),
	  { \parskip('0pt'),
	    \tableofcontents
	  },
	  \pagebreak
	].
		    
section(s1, section).
section(s2, subsection).
section(s3, subsubsection).
section(s4, paragraph).

tex(S, A, Content) -->
	{ section(S, H), !,
	  Content = [ element(title, _, Title),
		      element(_,     _, Body)
		    ],
	  Sec =.. [H,Title]
	},
	[ \Sec
	],
	label(A),
	Body.

label(Attr) -->
	{ member(name=Name, Attr),
	  atom_concat('sec:', Name, Label)
	},
	[ \label(#Label)
	].
label(_) -->
	[].

tex(fn, _, Content) -->
	[ \footnote(Content)
	].

tex(quote, _, Content) -->
	[ env(quote, Content)
	].

tex(p, _, Content) -->
	[ \par
	| Content
	].
tex(br, _, []) -->
	[ \(\)
	].

tex(em, _, Content) -->
	[ \emph(Content)
	].
tex(b, _, Content) -->
	[ { [\bf | Content]}
	].


tex(c, _, Content) -->
	[ \index([{[\tt | Content]}]),
	  {[\tt | Content]}
	].
tex(verb, _, [Content]) -->
	[ \verb(Content)
	].
tex(code, _, [Code]) -->
	[ env(verbatim, Code)
	].
tex(xmp, A, XMP) -->
	{ member(placement=block, A)
	},
	[ env(quote,
	      [ \obeylines,
		\obeyspaces,
		\tt
	      | XMP
	      ])
	].
tex(xmp, _, XMP) -->
	[ {[ \tt | XMP ]}
	].

tex(ref, A, [To]) -->
	{ member(type=sec, A)
	},
	[ \tex('section~'),
	  \ref(['sec:', To])
	].
tex(ref, _, To) -->
	[ {[\bf | To]}
	].
tex(sh, _, Cmd) -->
	[ {[\tt | Cmd]}
	].
tex(elem, _, Elem) -->
	[ {[\bf | Elem]}
	].
tex(aref, _, To) -->
	[ {[\it | To]}
	].
tex(pref, _,
    [ element(name, _, [Name]),
      element(arity, _, [Arity])
    ]) -->
	[ {\bf, Name, /, Arity}
	].
tex(var, _, Content) -->
	[ \emph(Content)
	].
tex(jargon, _, Content) -->
	[ \index(Content),
	  \emph(Content)
	].


		 /*******************************
		 *	       LISTS		*
		 *******************************/

tex(ul, _, Content) -->
	[ env(itemize, Content)
	].
tex(item, _, [Head, Body]) -->
	[ \item,
	  Head,
	  Body
	].
tex(tag, _, Tag) -->
	[ \emph(Tag), \tex('\\mbox{}\\\\')
	].

		 /*******************************
		 *	    DEFINITIONS		*
		 *******************************/

tex(definitions, _, Content) -->
	[ env(description, Content)
	].
tex(definition, _, [Head|Rest]) -->
	{ xml_select(Rest, element(desc), Body)
	},
	[ \item(?[Head]), \tex('\\mbox{}\\\\'),
	  Body
	].
tex(pred, _, Content) -->
	{ xml_select(Content, element(name), element(_, _, [Name])),
	  xml_select(Content, element(arglist), ArgList)
	},
	[ {\bf, Name}
	],
	arglist(ArgList, Arity),
	[ \index([Name, /, Arity])
	].
	  
arglist(element(arglist, _, Content), Arity) -->
	{ findall(A,
		  xml_select(Content, element(arg), element(_,_,[A])),
		  Args),
	  length(Args, Arity)
	},
	(   { Arity == 0 }
	->  []
	;   [ {\bf, '('},
	      \tex('{\\it ')
	    ],
	    arglist(Args),
	    [ \tex('}'),
	      {\bf, ')'}
	    ]
	).
	  
arglist([]) -->
	[].
arglist([H]) --> !,
	[H].
arglist([H|T]) -->
	[ H, ', ' ],
	arglist(T).

tex(argdef, _, Content) -->
	[ env(description, Content)
	].
tex(valdef, _, Content) -->
	[ env(description, Content)
	].
tex(argval, _, [Head, Body]) -->
	[ \item(?[Head]), \tex('\\mbox{}\\\\'),
	  Body
	].
tex(termitem, _, Content) -->
	(   { xml_select(Content, element(arglist), ArgList),
	      xml_select(Content, element(name), element(_, _, [Name]))
	    }
	->  [ {\bf, Name}
	    ],
	    arglist(ArgList, _Arity)
	;   Content
	).
tex(term, _, Content) -->	% TBD: fontify
	[ {[\tt | Content]}
	].


		 /*******************************
		 *	      SUMMARY		*
		 *******************************/

summary(Content) -->
	{ setof(S, summary(Content, S), Summary)
	},
	[ \pagebreak, \n,
	  \appendix, \n,
	  \section(['Summary of Predicates']),
	  env(longtable(?[l], [ll]), Summary)
	].


summary(Content, element(summary_entry, [], [Pred, Summary])) :-
	xml_select(Content, element(definition), Def),
	(   xml_select(Def, element(/pred),      Pred),
	    xml_select(Def, element(/summary),   Summary)
	->  true
	).

tex(summary_entry, _, [Pred, Summary]) -->
	{ xml_select(Pred, element(name), element(_, _, [Name])),
	  xml_select(Pred, element(arglist), ArgList),
	  findall(A,
		  xml_select(ArgList, element(arg), element(_,_,[A])),
		  Args),
	  length(Args, Arity)
	},
	[ {\bf, Name, /, Arity},
	  \tex('&'),
	  Summary,
	  \tex(\\),
	  \n
	].


		 /*******************************
		 *	      CATCH ALL		*
		 *******************************/

tex(E, _, Content) -->
	{   pass(E)
	->  true
	;   format(user_error, 'Warning: Ignored element ~w holding ~p~n',
		   [E, Content])
	},
	Content.

pass(desc).
pass(name).
pass(summary).

		 /*******************************
		 *	       EMIT		*
		 *******************************/

emit_tex([]) :- !.
emit_tex([H|T]) :-
	etex(H), !,
	emit_tex(T).
emit_tex([H|T]) :-
	format(user_error, 'Error: failed to emit_tex on ~p~n', [H]),
	emit_tex(T).

etex(\par) :- !,
	format('~n~n', []).
etex(\parskip(X)) :-
	atom(X), !,
	format('\\parskip~w~n', [X]).
etex(\parindent(X)) :-
	atom(X), !,
	format('\\parindent~w~n', [X]).
etex(\br) :-
	format('\\\\', []).
etex(\br(Skip)) :-
	format('\\\\[~w]', [Skip]).
etex(\n(N)) :- !,
	forall(between(1, N, _), nl).
etex(\n) :- !,
	nl.
etex(\tex(Text)) :- !,
	format('~w', [Text]).
etex(\aml(AML)) :- !,
	to_tex(AML, TeX),
	emit_tex(TeX).
etex(\verb(Verb)) :- !,
	(   member(VC, "$!|/"),
	    atom_codes(Verb, Chars),
	    \+ member(VC, Chars)
	->  format('\\verb~c~w~c', [VC, Verb, VC])
	;   format(user_error, 'Error: no verb character for ~w~n', [Verb])
	).
etex(\Term) :- !,
	Term =.. [Name|Args],
	format('\\~w', [Name]),
	(   Args == []
	->  format(' ', [])
	;   emit_args(Args)
	).
etex(env(verbatim, Content)) :- !,
	format('~n~n\\begin{verbatim}~n', []),
	format('~w', [Content]),
	format('~n\\end{verbatim}~n~n', []).
etex(env(Term, Content)) :- !,
	Term =.. [Env|Args],
	Begin =.. [begin, #Env|Args],
	nl, nl,
	etex(\Begin),
	nl,
	to_tex(Content, TeX),
	emit_tex(TeX),
	nl,
	etex(\end(#Env)),
	nl, nl.
etex({G}) :- !,
	format('{'),
	(   is_list(G)
	->  L = G
	;   group_to_list(G, L)
	),
	to_tex(L, TeX),
	emit_tex(TeX),
	format('}').
etex(Atom) :-
	atomic(Atom), !,
	atom_chars(Atom, Chars),
	emit_tex_chars(Chars).
etex(Term) :-
	format('Error: failed to emit ~p~n', [Term]).

group_to_list((A,B), [A|T]) :- !,
	group_to_list(B, T).
group_to_list(A, [A]).

emit_args([]).
emit_args([H|T]) :-
	emit_arg(H),
	emit_args(T).

emit_arg(?Arg) :- !,
	format('['),
	emit_arg_val(Arg),
	format(']').
emit_arg(tex(Val)) :- !,
	format('~w', [Val]).
emit_arg(Arg) :- !,
	format('{'),
	emit_arg_val(Arg),
	format('}').

emit_arg_val(#Atom) :- !,
	format('~w', [Atom]).
emit_arg_val(H) :-
	to_tex(H, TeX),
	emit_tex(TeX).

emit_tex_chars([]).
emit_tex_chars([H|T]) :-
	emit_tex_char(H),
	emit_tex_chars(T).

emit_tex_char('$') :- !,
	format('\\$', []).
emit_tex_char('#') :- !,
	format('\\#', []).
emit_tex_char('&') :- !,
	format('\\&', []).
emit_tex_char('{') :- !,
	format('\\{', []).
emit_tex_char('}') :- !,
	format('\\}', []).
emit_tex_char('_') :- !,
	format('\\_', []).
emit_tex_char('|') :- !,
	format('{\\tt\\string|}', []).
emit_tex_char(C) :-
	format('~w', [C]).


