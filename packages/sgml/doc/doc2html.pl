/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(doc2html,
	  [ doc2html/1,			% +SGMLFile
	    x/0,			% convert sgml2pl.sgml
	    v/0				% Preview it
	  ]).
:- use_module(xml_convert).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Convert an SGML document into HTML. This  serves two purposes. The first
is to get HTML version for the documentation   and  the second is to see
how best to convert SGML documents to   textual formats using the Prolog
`DOM' representation.

The output of the grammar rule transformation  is a mixture of atoms and
codes. This has been choosen instead of simply a list of codes to reduce
stack-usage.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- op(500, fx, *).
:- op(500, fx, &).


doc2html(In) :-
	absolute_file_name(In,
			   [ extensions([sgml]),
			     access(read)
			   ], InFile),
	file_name_extension(Base, _, InFile),
	file_name_extension(Base, html, HTMLFile),
	load_sgml_file(InFile, Term),
	del(_),
	phrase(content(Term), HTML),
	open(HTMLFile, write, Out),
	write_doc(HTML, Out),
	close(Out).
	
write_doc([], _).
write_doc([nl(N)|T0], Out) :- !,
	join_nl(N, Lines, T0, T),
	write_lines(Lines, Out),
	write_doc(T, Out).
write_doc([H|T], Out) :-
	(   atom(H)
	->  write(Out, H)
	;   put_code(Out, H)
	),
	write_doc(T, Out).

write_lines(N, Out) :-
	(   N > 0
	->  N1 is N - 1,
	    nl(Out),
	    write_lines(N1, Out)
	;   true
	).

join_nl(N0, N, [nl(N2)|T0], T) :- !,
	N1 is max(N0, N2),
	join_nl(N1, N, T0, T).
join_nl(N, N, T, T).


x :-
	doc2html(sgml2pl).

v :-
	shell('netscape file:`pwd`/sgml2pl.html &').


		 /*******************************
		 *	      STORAGE		*
		 *******************************/

:- dynamic
	stored/1.

set(Value) :-
	asserta(stored(Value)).
del(Value) :-
	retractall(stored(Value)).


:- discontiguous
	html/5.

html(element(E,A,C)) -->
	html(E,A,C).


html(pldoc, _, C0) -->
	{ xml_select(C0, element(titlepage/title), element(_,_,Title)),
	  mktoc(C0, C1, Toc),
	  set(toc(Toc)),
	  mkindex(C1, C2, Index),
	  mkfn(C2, C, Footnotes)
	},
	[ '<!DOCTYPE PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n\n'
	],
	starttag(html),
	\head(\title(Title)),
	starttag(body),
	content(C),
	summary(C0),
	html(Footnotes),
	html(Index),
	endtag(body),
	endtag(html).


		 /*******************************
		 *	  TABLEOFCONTENT	*
		 *******************************/

mktoc(C0, C, element(ul, [], Toc)) :-
	mktoc(C0, C, 'sec-', 0, Toc).

mktoc([], [], _, _, []).
mktoc([element(E, A0, C0)|T0], Doc, Prefix, N, Toc) :-
	section(E, _, _), !,		% this is a section
	Doc   = [element(E, A, C)|T],
	Toc   = [element(li, [href=HREF], TheTitle)|CT],
	C0    = [Title, element(B, BA, Body0)],
	C     = [Title, element(B, BA, Body)],
	Title = element(_,_,TheTitle),
	N1 is N + 1,
	mksubid(Prefix, N1, SecId),
	(   member(name=RN, A0)
	->  A = A0
	;   RN = SecId,
	    A = [name=RN|A0]
	),
	atom_concat('#', RN, HREF),
	mktoc(Body0, Body, SecId, 0, SubToc),
	(   SubToc == []
	->  mktoc(T0, T, Prefix, N1, CT)
	;   CT = [element(ul, [], SubToc)|CT1],
	    mktoc(T0, T, Prefix, N1, CT1)
	).
mktoc([H|T0], [H|T], Pre, N, Toc) :-
	mktoc(T0, T, Pre, N, Toc).

mksubid('sec-', N, Id) :- !,
	atom_concat('sec-', N, Id).
mksubid(Pre, N, Id) :-
	concat_atom([Pre, '.', N], Id).


		 /*******************************
		 *	       INDEX		*
		 *******************************/

%	index(Element, Ref, SortKey-Label)
%
%	Index this element.  Ref is the URI, Label is the label for the index

index(element(c, A, [C]), Ref, C-element(c, A, [C])) :-
	atom_concat('const:', C, Ref).
index(element(pred, _, C), Ref, Ref-element(b, [], [Ref])) :-
	xml_select(C, element(/name), element(_,_,[Name])),
	findall(A, xml_select(C, element(/arglist/arg), A), AL),
	length(AL, Arity),
	concat_atom([Name, /, Arity], Ref).

:- dynamic
	index_key/2.

mkkey(Key0, Key) :-
	retract(index_key(Key0, I)), !,
	concat_atom([Key0, '%', I], Key),
	I1 is I + 1,
	assert(index_key(Key0, I1)).
mkkey(Key, Key) :-
	assert(index_key(Key, 2)).


mkindex(C0, C, element(index, [], Index)) :-
	retractall(index_key(_,_)),
	phrase(mkindex(C0, C), Index0),
	keysort(Index0, Index1),
	unkey(Index1, Index).

unkey([], []).
unkey([_-H|T0], [H|T]) :-
	unkey(T0, T).

mkindex([], []) -->
	[].
mkindex([H0|T0], [H|T]) -->
	{ index(H0, Ref0, SortKey-Label), !,
	  mkkey(Ref0, Ref),
	  H0 = element(E, A, C0),
	  H  = element(E, [name=Ref|A], C),
	  atom_concat('#', Ref, HREF)
	},
	[ SortKey-element(index_entry, [href=HREF], Label)
	],
	mkindex(C0, C),
	mkindex(T0, T).
mkindex([element(E,A,C0)|T0], [element(E,A,C)|T]) -->
	mkindex(C0, C),
	mkindex(T0, T).
mkindex([H|T0], [H|T]) -->
	mkindex(T0, T).

html(index, _, Index) -->
	\h1('Index'),
	starttag(ul),
	print_index(Index),
	endtag(ul).
	
print_index([]) -->
	"".
print_index([H|T0]) -->
	{ H = element(index_entry, A, Label)
	},
	\li(\a(A, Label)),
	(   { T0 = [element(_,_,Label)|_]
	    }
	->  " [",
	    ct_index(Label, 2, T0, T),
	    "]",
	    print_index(T)
	;   print_index(T0)
	).

ct_index(_, _, [], []) --> !,
	"".
ct_index(Label, N, [element(index_entry, A, Label)|T0], T) --> !,
	{ N1 is N + 1
	},
	" ",
	\a(A, N),
	ct_index(Label, N1, T0, T).
ct_index(_, _, T, T) -->
	"".


		 /*******************************
		 *	      SUMMARY		*
		 *******************************/

summary(Content) -->
	{ setof(S, summary(Content, S), Summary)
	},
	\h1(\a([name='Summary'], ['Summary of Predicates'])),
	\table([border=2], Summary).


summary(Content, element(summary_entry, [], [Pred, Summary])) :-
	xml_select(Content, element(definition), Def),
	(   xml_select(Def, element(/pred),      Pred),
	    xml_select(Def, element(/summary),   Summary)
	->  true
	).

html(summary_entry, _, [Pred, Summary]) -->
	{ xml_select(Pred, element(name), element(_, _, [Name])),
	  xml_select(Pred, element(arglist), ArgList),
	  findall(A,
		  xml_select(ArgList, element(arg), element(_,_,[A])),
		  Args),
	  length(Args, Arity),
	  concat_atom([#, Name,/,Arity], Ref)
	},
	\tr([\td(\a([href=Ref], \b([Name,/,Arity]))), \td(Summary)]).

html(summary, _, Content) -->
	content(Content).


		 /*******************************
		 *	     FOOTNOTES		*
		 *******************************/

mkfn(C0, C, element(footnotes, [], Notes)) :-
	phrase(mkfn(C0, C, 1, _), Notes).

mkfn([], [], N, N) -->
	[].
mkfn([element(fn, A, Note)|T0],
     [element(fnmark, [name=FN, id=N0|A], [])|T], N0, N) -->
	{ N1 is N0 + 1,
	  atom_concat('fn-', N0, FN)
	},
	[ element(fntext, [name=FN, id=N0|A], Note)
	],
	mkfn(T0, T, N1, N).
mkfn([element(E,A,C0)|T0], [element(E,A,C)|T], N0, N) -->
	mkfn(C0, C, N0, N1),
	mkfn(T0, T, N1, N).
mkfn([H|T0], [H|T], N0, N) -->
	mkfn(T0, T, N0, N).

html(fnmark, A, []) -->
	{ memberchk(name=Name, A),
	  memberchk(id=Id, A),
	  atom_concat(#, Name, Ref),
	  atom_concat('txt:', Name, TxtName)
	},
	\a([href=Ref, name=TxtName], \sup(Id)).

html(footnotes, _, Notes) -->
	\h1(['Footnotes']),
	\dl(Notes).
html(fntext, A, Note) -->
	{ memberchk(name=Name, A),
	  memberchk(id=Id, A),
	  atom_concat('#txt:', Name, Ref)
	},
	\dt(\a([href=Ref, name=Name], Id)),
	\dd(Note).


		 /*******************************
		 *	       HEAD		*
		 *******************************/

html(head, _, Content) -->
	content(Content).
html(version, _, Content) -->
	{ set(version(Content))
	}.
html('release-date', _, Content) -->
	{ set(release_date(Content))
	}.
html(package, _, Content) -->
	{ set(package(Content))
	}.


		 /*******************************
		 *	     TITLEPAGE		*
		 *******************************/

html(titlepage, _, Content) -->
	{ xml_select(Content, element(title), element(_, _, Title)),
	  xml_select(Content, element(author/name), element(_, _, Name)),
	  xml_select(Content, element(author/address), element(_, _, Address)),
	  xml_select(Content, element(author/email), element(_, _, Email)),
	  xml_select(Content, element(abstract), element(_, _, Abstract)),

	  stored(version(Version)),
	  stored(release_date(Date)),
	  stored(toc(Toc))
	},
	\center([ \h1([ *Title, \br,
			\font([size='+0'],
			      \em(['Version ', *Version, ', ', *Date]))
		      ]), \br,
		  \a([href=['mailto:'|Email]], \em(Name)), \br,
		  \em(Address)
		]),
	\blockquote([ \hr,
		      *Abstract,
		      \hr
		    ]),
	content([ \h1('Table of Content'),
		  Toc
		]).

		    
		 /*******************************
		 *	     SECTIONS		*
		 *******************************/

section(s1, h1, [/*align=center*/]).
section(s2, h2, []).
section(s3, h3, []).
section(s4, h4, []).

html(S, A, Content) -->
	{ section(S, H, SA), !,
	  Content = [ element(title, _, Title),
		      element(_, _, Body)
		    ]
	},
	\p(&nbsp),
	starttag(H, SA),
	(   { member(name=Ref, A)
	    }
	->  \a([name=Ref], Title)
	;   content(Title)
	),
	endtag(H),
	content(Body).



		 /*******************************
		 *	       BASICS		*
		 *******************************/

html(br, _, []) -->
	\br.
html(p, _, Content) -->
	\p(Content).
html(b, _, Content) -->
	\b(Content).
html(em, _, Content) -->
	\em(Content).
html(quote, _, Content) -->
	\blockquote(Content).


		 /*******************************
		 *	       PROLOG		*
		 *******************************/

html(ref, _, Ref) -->
	\b(Ref).
html(c, A, Content) -->
	{ member(name=Ref, A)
	}, !,
	\a([name=Ref], \b(\tt(Content))).
html(c, _, Content) -->
	\b(\tt(Content)).
html(elem, _, Content) -->
	\b(\tt(Content)).
html(jargon, _, Content) -->
	\b(\em(Content)).
html(term, _, Content) -->
	\code(Content).
html(sh, _, Content) -->
	\code(Content).
html(pref, A, [element(_,_,[Name]), element(_,_,[Arity])]) -->
	{ member(type=builtin, A)
	},
	\b([Name, /, Arity]).
html(pref, _, [element(_,_,[Name]), element(_,_,[Arity])]) -->
	{ concat_atom([#, Name, /, Arity], Ref)
	},
	\a([href=Ref], \b([Name, /, Arity])).
html(verb, _, Content) -->
	\code(Content).
html(aref, _, Content) -->
	\var(Content).
html(var, _, Content) -->
	\var(Content).
html(xmp, A, Content) -->
	{ member(placement=block, A)
	}, !,
	\p,
	\pre(Content).
html(xmp, _, Content) -->
	\code(Content).
html(code, _, Content) -->
	\p,
	\table([ width='90%',
		 align=center,
		 border=2,
		 bgcolor='#f0f0f0'
	       ],
	       [ \tr(\td(\pre(Content)))
	       ]).


		 /*******************************
		 *	    DEFINITIONS		*
		 *******************************/

html(definitions, _, Content) -->
	\dl(Content).
html(definition, _, [Head|Rest]) -->
	{ xml_select(Rest, element(desc), Body)
	},
	\dt([&nbsp, \br, \b(Head)]),
	\dd(Body).
html(argdef, _, Content) -->
	\dl(Content).
html(valdef, _, Content) -->
	\dl(Content).

%	Predicates

html(desc, _, Body) -->
	content(Body).
html(pred, A, [Name, Arglist]) -->
	{ member(name=Ref, A)
	}, !,
	content(\a([name=Ref], [Name, '(', \var(Arglist), ')'])).
html(pred, _, [Name, Arglist]) -->
	content([Name, '(', \var(Arglist), ')']).
html(arglist, _, Args) -->
	arglist(Args).

arglist(Args) -->
	arglist(Args, _).

arglist(Args, Arity) -->
	arglist(Args, 0, Arity).

arglist([], A, A) -->
	"".
arglist([Last], A0, A) --> !,
	{ A is A0+1
	},
	content(Last).
arglist([H|T], A0, A) -->
	{ A1 is A0+1
	},
	content([H, ', ']),
	arglist(T, A1, A).

html(arg, _, Arg) -->
	content(Arg).
html(name, _, Name) -->
	content(Name).

%	Arguments

html(argval, _, [Head, Body]) -->
	\dt([&nbsp, \br,\b(Head)]),
	\dd(Body).
html(termitem, _, [element(name, _, Name), Arglist]) -->
	content([*Name, '(', \var(Arglist), ')']).
html(termitem, _, Content) -->
	content(Content).

		 /*******************************
		 *	       LISTS		*
		 *******************************/

html(ul, _, Content) -->
	\ul(Content).
html(li, A, Content) -->
	{ member(href=Ref, A)
	}, !,
	\li(\a([href=Ref], Content)).
html(li, _, Content) -->
	\li(Content).
html(item, _, Content) -->
	\li(Content).
html(tag, _, Tag) -->
	\em(Tag),
	\br.

		 /*******************************
		 *	       EMIT		*
		 *******************************/

\Term -->
	{ Term =.. [Name|Args]
	},
	element(Args, Name).

element([], Name) -->
	starttag(Name, []),
	endtag(Name).
element([Content], Name) --> !,
	starttag(Name, []),
	content(Content),
	endtag(Name).
element([Args, Content], Name) -->
	starttag(Name, Args),
	content(Content),
	endtag(Name).
	
starttag(Name) -->
	starttag(Name, []).
starttag(Name, Args) -->
	lines(Name, start(before)),
	"<",
	atom(Name),
	args(Args),
	">",
	lines(Name, start(after)).

endtag(Name) -->			% omitted end-tag
	{ layout(Name, _, -)
	}, !.
endtag(Name) -->
	lines(Name, end(before)),
	"</",
	atom(Name),
	">",
	lines(Name, end(after)).

args([]) -->
	"".
args([Name=Value|T]) --> !,
	" ",
	atom(Name),
	"=\"",
	content(Value),
	"\"",
	args(T).
args([Name|T]) -->
	" ",
	atom(Name),
	args(T).

atom(Name) -->
	[ Name
	].

value(Value) -->
	{ integer(Value), !,
	  number_codes(Value, Codes)
	},
	Codes.
value(Value) -->
	{ atom_codes(Value, Chars),
	  member(C, Chars),
	  escape_code(C, _), !
	},
	escape(Chars).
value(Value) -->
	[ Value
	].

escape([]) -->
	[].
escape([H|T]) -->
	(   { escape_code(H, Entity)
	    }
	->  [Entity]
	;   [H]
	),
	escape(T).

escape_code(0'<, '&lt;').
escape_code(0'>, '&gt;').
escape_code(0'&, '&amp;').

lines(Tag, start(before)) -->
	{ layout(Tag, N-_, _)
	}, !,
	lines(N).
lines(Tag, start(after)) -->
	{ layout(Tag, _-N, _)
	}, !,
	lines(N).
lines(Tag, end(before)) -->
	{ layout(Tag, _, N-_)
	}, !,
	lines(N).
lines(Tag, end(after)) -->
	{ layout(Tag, _, _-N)
	}, !,
	lines(N).
lines(_, _) -->
	"".

lines(N) -->
	[ nl(N)
	].

layout(p,	   2-1, -).
layout(br,	   0-1, -).
layout(hr,	   1-1, -).
layout(h1,	   2-0,	0-2).
layout(h2,	   2-0,	0-2).
layout(h3,	   2-0,	0-2).
layout(h4,	   2-0,	0-2).
layout(blockquote, 2-1,	1-2).
layout(center,	   2-1,	1-2).
layout(head,	   1-1,	1-1).
layout(body,	   2-1,	1-1).
layout(ul,	   1-1,	1-1).
layout(dl,	   1-1,	1-1).
layout(dt,	   1-0, -).
layout(dd,	   0-1, -).
layout(li,	   1-0,	-).
layout(pre,	   1-1, 1-1).
layout(table,	   1-1, 1-1).
layout(tr,	   1-0, 0-1).

content([]) --> !,
	[].
content([H|T]) --> !,
	(   content1(H)
	->  []
	;   { format(user_error, 'Skipped ~p~n', [H]) }
	),
	content(T).
content(H) -->
	content([H]).

content1(*List) --> !,
	content(List).
content1(\Term) --> !,
	\Term.
content1(&Ent) --> !,
	"&",
	atom(Ent),
	";".
content1(element(E,A,C)) --> !,
	html(E,A,C).
content1(CDATA) -->
	{ atomic(CDATA)
	}, !,
	value(CDATA).
	

		 /*******************************
		 *	      PORTRAY		*
		 *******************************/

user:portray(element(E,_,_)) :-
	format('<~w>...<~w>', [E, E]).
user:portray(String) :-
	code_string(String, 0, Len),
	Len > 3,
	format('"~s"', [String]).

code_string([], N, N).
code_string([H|T], N0, N) :-
	integer(H),
	(   code_type(H, graph)
	;   code_type(H, space)
	), !,
	N1 is N0+1,
	code_string(T, N1, N).
