/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amsterdam. All rights reserved.
*/

:- module(html_write,
	  [ page/4,			% generate an HTML page
	    html/3,

					% Useful primitives for expanding
	    html_begin/3,		% +EnvName[(Attribute...)]
	    html_end/3,			% +EnvName
	    html_quoted/3,		% +Text
	    html_quoted_attribute/3,	% +Attribute

					% Emitting the HTML code
	    print_html/1,		% +List
	    print_html/2,		% +Stream, +List
	    html_print_length/2		% +List, -Length
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(html_write)

The purpose of this library  is  to   simplify  writing  HTML  pages. Of
course, it is posible to use format/[2,3]   to  write to the HTML stream
directly, but this is generally not very satisfactory:

	* It is a lot of typing
	* It does not guarantee proper HTML syntax.  You have to deal
	  with HTML quoting, proper nesting and reasonable layout.
	* It is hard to use satisfactory abstraction

This module tries to remedy these problems.   The idea is to translate a
Prolog term into  an  HTML  document.  We   use  DCG  for  most  of  the
generation. 
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

page(Head, Contents) -->
	[ '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 4.0//EN">\n',
	  '<html>\n',
	  '<head>',
	  nl(1)
	],
	html(Head),
	[ '</head>\n\n',
	  '<body bgcolor=white>\n'
	],
	html(Contents),
	[ nl(1),
	  '</body>\n',
	  '</html>\n'
	].


html([]) --> !,
	[].
html([H|T]) --> !,
	(   do_expand(H)
	->  []
	;   { print_message(error, html(expand_failed(H)))
	    }
	),
	html(T).
html(X) -->
	do_expand(X).

:- multifile
	expand/3.

do_expand(Token) -->			% call user hooks
	expand(Token), !.
do_expand(Token) -->
	{ atomic(Token)
	}, !,
	html_quoted(Token).
do_expand(Fmt-Args) --> !,
	{ sformat(String, Fmt, Args)
	},
	html_quoted(String).
do_expand(Module:Term, In, Rest) :- !,
	call(Module:Term, In, Rest).
do_expand(Term) -->
	{ Term =.. [Env, Contents]
	}, !,
	(   { layout(Env, _, empty)
	    }
	->  html_begin(Env, Contents)
	;   html_begin(Env),
	    html(Contents),
	    html_end(Env)
	).
do_expand(Term) -->
	{ Term =.. [Env, Attributes, Contents]
	}, !,
	html_begin(Env, Attributes),
	html(Contents),
	html_end(Env).

	
html_begin(Env) -->
	{ Env =.. [Name|Attributes]
	},
	html_begin(Name, Attributes).

html_begin(Env, Attributes) -->
	pre_open(Env),
	[<],
	[Env],
	attributes(Attributes),
	[>],
	post_open(Env).

html_end(Env)   -->			% empty element or omited close
	{ layout(Env, _, -)
	}, !,
	[].
html_end(Env)   -->
	pre_close(Env),
	['</'],
	[Env],
	['>'],
	post_close(Env).

attributes([]) --> !,
	[].
attributes([H|T]) --> !,
	attribute(H),
	attributes(T).
attributes(One) -->
	attribute(One).

attribute(Name=Value) --> !,
	[' ', Name, '="' ],
	html_quoted_attribute(Value),
	['"'].
attribute(Term) -->
	{ Term =.. [Name, Value]
	}, !,
	attribute(Name=Value).
attribute(Atom) -->			% Value-abbreviated attribute
	{ atom(Atom)
	},
	[ ' ', Atom ].


		 /*******************************
		 *	   QUOTING RULES	*
		 *******************************/

%	html_quoted(Text)
%
%	Quote the value for normal text.

html_quoted(Text) -->
	{ sub_atom(Text, _, _, _, <)
	; sub_atom(Text, _, _, _, >)
	; sub_atom(Text, _, _, _, &)
	}, !,
	{ atom_chars(Text, Chars),
	  quote_chars(Chars, QuotedChars),
	  concat_atom(QuotedChars, Quoted)
	},
	[ Quoted ].
html_quoted(Text) -->
	[ Text ].

quote_chars([], []).
quote_chars([H0|T0], [H|T]) :-
	quote_char(H0, H),
	quote_chars(T0, T).

quote_char(<, '&lt;') :- !.
quote_char(>, '&gt;') :- !.
quote_char(&, '&amp;') :- !.
quote_char(X, X).

%	html_quoted_attribute(+Text)
%
%	Quote the value according to the rules for tag-attributes

html_quoted_attribute(Text) -->
	{ sub_atom(Text, _, _, _, <)
	; sub_atom(Text, _, _, _, >)
	; sub_atom(Text, _, _, _, &)
	; sub_atom(Text, _, _, _, '"')
	; sub_atom(Text, _, _, _, '''')
	}, !,
	{ atom_chars(Text, Chars),
	  quote_att_chars(Chars, QuotedChars),
	  concat_atom(QuotedChars, Quoted)
	},
	[ Quoted ].
html_quoted_attribute(Text) -->
	[ Text ].
	
quote_att_chars([], []).
quote_att_chars([H0|T0], [H|T]) :-
	quote_att_char(H0, H),
	quote_att_chars(T0, T).

quote_att_char(<, '&lt;') :- !.
quote_att_char(>, '&gt;') :- !.
quote_att_char(&, '&amp;') :- !.
quote_att_char('"', '&quot;') :- !.
quote_att_char('''', '&apos;') :- !.
quote_att_char(X, X).


		 /*******************************
		 *	       LAYOUT		*
		 *******************************/

pre_open(Env) -->
	{ layout(Env, N-_, _)
	}, !,
	[ nl(N) ].
pre_open(_) --> [].

post_open(Env) -->
	{ layout(Env, _-N, _)
	}, !,
	[ nl(N) ].
post_open(_) -->
	[].

pre_close(Env) -->
	{ layout(Env, _, N-_)
	}, !,
	[ nl(N) ].
pre_close(_) -->
	[].

post_close(Env) -->
	{ layout(Env, _, _-N)
	}, !,
	[ nl(N) ].
post_close(_) -->
	[].

%	layout(Tag, PreOpen-PostOpen, PreClose-PostClose)
%
%	Define required newlines before and after tags.  Thus table is
%	rather incomplete.

layout(table,	   2-1,	1-2).
layout(blockquote, 2-1,	1-2).
layout(center,	   2-1,	1-2).
layout(dl,	   2-1,	1-2).
layout(ul,	   2-1,	1-2).
layout(form,	   2-1,	1-2).

layout(head,	   1-1,	1-1).
layout(body,	   1-1,	1-1).

layout(tr,	   1-0,	0-1).
layout(title,	   1-0,	0-1).

layout(h1,	   2-0,	0-2).
layout(h2,	   2-0,	0-2).
layout(h3,	   2-0,	0-2).
layout(h4,	   2-0,	0-2).

layout(hr,	   1-1, empty).		% empty elements
layout(br,	   0-1, empty).
layout(img,	   0-0, empty).
layout(meta,	   1-1, empty).
layout(base,	   1-1, empty).
layout(link,	   1-1, empty).
layout(input,	   0-0, empty).

layout(p,	   2-1, -).		% omited close
layout(td,	   0-0, -).


		 /*******************************
		 *	     PRINTING		*
		 *******************************/

%	print_html(+Stream, +List)
%
%	Print list of atoms and layout instructions.  Currently used layout
%	instructions:
%
%		nl(N)	Use at minimum N newlines here.

print_html(List) :-
	current_output(Out),
	write_html(List, Out).
print_html(Out, List) :-
	write_html(List, Out).

write_html([], _).
write_html([nl(N)|T], Out) :- !,
	join_nl(T, N, Lines, T2),
	write_nl(Lines, Out),
	write_html(T2, Out).
write_html([H|T], Out) :-
	write(Out, H),
	write_html(T, Out).

join_nl([nl(N0)|T0], N1, N, T) :- !,
	N2 is max(N0, N1),
	join_nl(T0, N2, N, T).
join_nl(L, N, N, L).

write_nl(0, _) :- !.
write_nl(N, Out) :-
	nl(Out),
	N1 is N - 1,
	write_nl(N1, Out).

%	html_print_length(+List, -Len)
%
%	Determine the content length of the list.

html_print_length(List, Len) :-
	html_print_length(List, 0, Len).

html_print_length([], L, L).
html_print_length([nl(N)|T], L0, L) :- !,
	join_nl(T, N, Lines, T1),
	L1 is L0 + Lines,		% assume only \n!
	html_print_length(T1, L1, L).
html_print_length([H|T], L0, L) :-
	atom_length(H, Hlen),
	L1 is L0+Hlen,
	html_print_length(T, L1, L).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(html(expand_failed(What))) -->
	[ 'Failed to translate to HTML: ~p'-[What] ].
