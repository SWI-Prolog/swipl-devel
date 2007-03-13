/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2007, University of Amsterdam

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

:- module(html_write,
	  [ reply_html_page/2,		% :Head, :Body

	    page/3,			% generate an HTML page
	    page/4,			% page from head and body
	    html/3,

	    html_set_options/1,		% +OptionList
	    html_current_option/1,	% ?Option

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
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(sgml)).		% Quote output
:- use_module(library(quintus)).	% for meta_predicate/1
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	reply_html_page(:, :),
	html(:, -, +),
	page(:, -, +),
	page(:, :, -, +),
	pagehead(:, -, +),
	pagebody(:, -, +).

/** <module> Write HTML text

The purpose of this library  is  to   simplify  writing  HTML  pages. Of
course, it is possible to  use  format/3   to  write  to the HTML stream
directly, but this is generally not very satisfactory:

	* It is a lot of typing
	* It does not guarantee proper HTML syntax.  You have to deal
	  with HTML quoting, proper nesting and reasonable layout.
	* It is hard to use satisfactory abstraction

This module tries to remedy these problems.   The idea is to translate a
Prolog term into  an  HTML  document.  We   use  DCG  for  most  of  the
generation.

---++ International documents

The library supports the generation of international documents, but this
is currently limited to using UTF-8 encoded HTML or XHTML documents.  It
is strongly recommended to use the following mime-type.

==
Content-type: text/html; charset=UTF-8
==

When generating XHTML documents, the output stream must be in UTF-8
encoding.
*/


		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

%%	html_set_options(+Options) is det.
%
%	Set options for the HTML output.   Options  are stored in prolog
%	flags to ensure  with  proper   multi-threaded  behaviour  where
%	setting an option is local to the   thread and new threads start
%	with the options from the parent thread.  Defined options are:
%	
%		* dialect(Dialect)
%		One of =html= (default) or =xhtml=.
%		
%		* doctype(+DocType)
%		Set the =|<|DOCTYPE|= DocType =|>|= line for page//1 and
%		page//2.
%		
%		* content_type(+ContentType)
%		Set the =|Content-type|= for reply_html_page/2
%		
%	Note  that  the  doctype  is  covered    by  two  prolog  flags:
%	=html_doctype= for the html dialect  and =xhtml_doctype= for the
%	xhtml dialect. Dialect muct be switched before doctype.

html_set_options([]).
html_set_options([H|T]) :-
	html_set_option(H),
	html_set_options(T).

html_set_option(dialect(Dialect)) :- !,
	must_be(oneof([html,xhtml]), Dialect),
	set_prolog_flag(html_dialect, Dialect).
html_set_option(doctype(Atom)) :- !,
	must_be(atom, Atom),
	(   current_prolog_flag(html_dialect, html)
	->  set_prolog_flag(html_doctype, Atom)
	;   set_prolog_flag(xhtml_doctype, Atom)
	).
html_set_option(content_type(Atom)) :- !,
	must_be(atom, Atom),
	(   current_prolog_flag(html_dialect, html)
	->  set_prolog_flag(html_content_type, Atom)
	;   set_prolog_flag(xhtml_content_type, Atom)
	).
html_set_option(O) :-
	domain_error(html_option, O).


%%	html_current_option(?Option) is nondet.
%
%	True if Option is an active option for the HTML generator.

html_current_option(dialect(Dialect)) :-
	current_prolog_flag(html_dialect, Dialect).
html_current_option(doctype(DocType)) :-
	(   current_prolog_flag(html_dialect, html)
	->  current_prolog_flag(html_doctype, DocType)
	;   current_prolog_flag(xhtml_doctype, DocType)
	).
html_current_option(content_type(ContentType)) :-
	(   current_prolog_flag(html_dialect, html)
	->  current_prolog_flag(html_content_type, ContentType)
	;   current_prolog_flag(xhtml_content_type, ContentType)
	).


option_default(html_dialect, html).
option_default(html_doctype, 'HTML PUBLIC "-//IETF//DTD HTML 4.0//EN"').
option_default(xhtml_doctype,
	       'html PUBLIC "-//W3C//DTD XHTML 1.0 \
	       Transitional//EN" \
	       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"').
option_default(html_content_type, 'text/html').
option_default(xhtml_content_type, 'application/xhtml+xml; charset=UTF-8').

%%	init_options is det.
%
%	Initialise the HTML processing options.

init_options :-
	(   option_default(Name, Value),
	    (	current_prolog_flag(Name, _)
	    ->	true
	    ;	set_prolog_flag(Name, Value)
	    ),
	    fail
	;   true
	).
	
:- initialization
	init_options.

%%	xml_header(-Header)
%
%	First line of XHTML document.  Added by print_html/1.

xml_header('<?xml version=\'1.0\' encoding=\'UTF-8\'?>').

%%	ns(?Which, ?Atom)
%
%	Namespace declarations

ns(xhtml, 'http://www.w3.org/1999/xhtml').


		 /*******************************
		 *	       PAGE		*
		 *******************************/

%%	page(+Content:dom)// is det.
%%	page(+Head:dom, +Body:dom)// is det.
%
%	Generate a page including the   HTML  =|<!DOCTYPE>|= header. The
%	actual doctype is read from the   option =doctype= as defined by
%	html_set_options/1.

page(Content) -->
	{ html_current_option(doctype(DocType)) },
	[ '<!DOCTYPE ', DocType, '>'
	],
	html(html(Content)).

page(Head, Body) -->
	{ html_current_option(doctype(DocType)) },
	[ '<!DOCTYPE ', DocType, '>'
	],
	html_begin(html),
	pagehead(Head),
	pagebody(Body),
	html_end(html).

pagehead(Head) -->
	{ functor(Head, head, _)
	}, !,
	html(Head).
pagehead(Head) -->
	{ strip_module(Head, M, _),
	  hook_module(M, head(_,_,_))
	}, !,
	M:head(Head).
pagehead(Head) -->
	html(head(Head)).


pagebody(Body) -->
	{ functor(Body, body, _)
	}, !,
	html(Body).
pagebody(Body) -->
	{ strip_module(Body, M, _),
	  hook_module(M, body(_,_,_))
	}, !,
	M:body(Body).
pagebody(Body) -->
	html(body([bgcolor(white)], Body)).


hook_module(M, P) :-
	current_predicate(_, M:P), !.
hook_module(user, P) :-
	current_predicate(_, user:P).

%%	html(+Content:dom)// is det
%
%	Generate HTML from Content.  Generates a token sequence for
%	print_html/2.

html(Spec) -->
	{ strip_module(Spec, M, T)
	},
	html(T, M).

html([], _) --> !,
	[].
html([H|T], M) --> !,
	(   do_expand(H, M)
	->  []
	;   { print_message(error, html(expand_failed(H)))
	    }
	),
	html(T, M).
html(X, M) -->
	do_expand(X, M).

:- multifile
	expand/3.

do_expand(Token, _) -->			% call user hooks
	expand(Token), !.
do_expand(Fmt-Args, _) --> !,
	{ format(string(String), Fmt, Args)
	},
	html_quoted(String).
do_expand(\List, _) -->
	{ is_list(List)
	}, !,
	List.
do_expand(\Term, Module, In, Rest) :- !,
	call(Module:Term, In, Rest).
do_expand(Module:Term, _, In, Rest) :- !,
	call(Module:Term, In, Rest).
do_expand(script(Content), _) --> !,	% general CDATA declared content elements?
	html_begin(script),
	[ Content
	],
	html_end(script).
do_expand(&(Entity), _) --> !,
	{ format(string(String), '&~w;', [Entity])
	},
	[ String ].
do_expand(Token, _) -->
	{ atomic(Token)
	}, !,
	html_quoted(Token).
do_expand(element(Env, Attributes, Contents), M) --> !,
	(   { Contents == [],
	      html_current_option(dialect(xhtml))
	    }
	->  xhtml_empty(Env, Attributes)
	;   html_begin(Env, Attributes),
	    html(Contents, M),
	    html_end(Env)
	).
do_expand(Term, M) -->
	{ Term =.. [Env, Contents]
	}, !,
	(   { layout(Env, _, empty)
	    }
	->  html_begin(Env, Contents)
	;   (   { Contents == [],
		  html_current_option(dialect(xhtml))
		}
	    ->  xhtml_empty(Env, [])
	    ;	html_begin(Env),
		html(Contents, M),
		html_end(Env)
	    )
	).
do_expand(Term, M) -->
	{ Term =.. [Env, Attributes, Contents]
	}, !,
	(   { Contents == [],
	      html_current_option(dialect(xhtml))
	    }
	->  xhtml_empty(Env, Attributes)
	;   { non_empty(Env, Term)
	    },
	    html_begin(Env, Attributes),
	    html(Contents, M),
	    html_end(Env)
	).

non_empty(Tag, Term) :-
	layout(Tag, _, empty), !,
	print_message(warning, format('Using empty element with content: ~p', [Term])).
non_empty(_, _).


%%	html_begin(+Env)// is det.
%%	html_end(+End)// is det
%
%	For  html_begin//1,  Env  is   a    term   Env(Attributes);  for
%	html_end//1  it  is  the  plain    environment  name.  Used  for
%	exceptional  cases.  Normal  applications    use   html//1.  The
%	following two fragments are identical, where we prefer the first
%	as it is more concise and less error-prone.
%	
%	==
%		html(table(border=1, \table_content))
%	==
%	==
%		html_begin(table(border=1)
%		table_content,
%		html_end(table)
%	==

html_begin(Env) -->
	{ Env =.. [Name|Attributes]
	},
	html_begin(Name, Attributes).

html_begin(Env, Attributes) -->
	pre_open(Env),
	[<],
	[Env],
	attributes(Env, Attributes),
	(   { layout(Env, _, empty),
	      html_current_option(dialect(xhtml))
	    }
	->  ['/>']
	;   [>]
	),
	post_open(Env).

html_end(Env)   -->			% empty element or omited close
	{ layout(Env, _, -),
	  html_current_option(dialect(html))
	; layout(Env, _, empty)
	}, !,
	[].
html_end(Env)   -->
	pre_close(Env),
	['</'],
	[Env],
	['>'],
	post_close(Env).

%%	xhtml_empty(+Env, +Attributes)// is det.
%
%	Emit element in xhtml mode with empty content.

xhtml_empty(Env, Attributes) -->
	pre_open(Env),
	[<],
	[Env],
	attributes(Attributes),
	['/>'].


%%	attributes(+Env, +Attributes)// is det.
%
%	Emit attributes for Env. Adds XHTML namespace declaration to the
%	html tag if not provided by the caller.

attributes(html, L) --> 
	{ html_current_option(dialect(xhtml)) }, !,
	(   { option(xmlns(_), L) }
	->  attributes(L)
	;   { ns(xhtml, NS) },
	    attributes([xmlns(NS)|L])
	).
attributes(_, L) -->
	attributes(L).

attributes([]) --> !,
	[].
attributes([H|T]) --> !,
	attribute(H),
	attributes(T).
attributes(One) -->
	attribute(One).

attribute(Name=Value) --> !,
	[' '], name(Name), [ '="' ],
	attribute_value(Value),
	['"'].
attribute(NS:Term) --> !,
	{ Term =.. [Name, Value]
	}, !,
	attribute((NS:Name)=Value).
attribute(Term) -->
	{ Term =.. [Name, Value]
	}, !,
	attribute(Name=Value).
attribute(Atom) -->			% Value-abbreviated attribute
	{ atom(Atom)
	},
	[ ' ', Atom ].

name(NS:Name) --> !,
	[NS, :, Name].
name(Name) -->
	[ Name ].

%%	attribute_value(+Value)
%
%	Print an attribute value.  Value  is   either  atomic  or a term
%	=|A+B|=, concatenating A and B.

attribute_value(Var) -->
	{ var(Var), !,
	  throw(error(instantiation_error, _))
	}.
attribute_value(A+B) --> !,
	attribute_value(A),
	attribute_value(B).
attribute_value(Value) -->
	html_quoted_attribute(Value).


		 /*******************************
		 *	   QUOTING RULES	*
		 *******************************/

%%	html_quoted(Text)// is det.
%
%	Quote  the  value  for  normal  (CDATA)  text.  Note  that  text
%	appearing in the document  structure   is  normally quoted using
%	these rules. I.e. the following emits  properly quoted bold text
%	regardless of the content of Text:
%	
%	==
%		html(b(Text))
%	==
%	
%	@tbd	Assumes UTF-8 encoding of the output.

html_quoted(Text) -->
	{ xml_quote_cdata(Text, Quoted, utf8) },
	[ Quoted ].

%%	html_quoted_attribute(+Text)// is det.
%
%	Quote the value  according  to   the  rules  for  tag-attributes
%	included in double-quotes.  Note   that  -like  html_quoted//1-,
%	attributed   values   printed   through   html//1   are   quoted
%	atomatically.
%	
%	@tbd	Assumes UTF-8 encoding of the output.

html_quoted_attribute(Text) -->
	{ xml_quote_attribute(Text, Quoted, utf8) },
	[ Quoted ].


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

%%	layout(+Tag, -Open, -Close) is det.
%
%	Define required newlines before and after   tags.  This table is
%	rather incomplete. New rules can  be   added  to  this multifile
%	predicate.
%	
%	@param Tag	Name of the tag
%	@param Open	Tuple M-N, where M is the number of lines before
%			the tag and N after.
%	@param Close	Either as Open, or the atom - (minus) to imit the
%			close-tag or =empty= to indicate the element has
%			no content model.
%			
% 	@tbd	Complete table

:- multifile
	layout/3.

layout(table,	   2-1,	1-2).
layout(blockquote, 2-1,	1-2).
layout(pre, 	   2-1,	1-2).
layout(center,	   2-1,	1-2).
layout(dl,	   2-1,	1-2).
layout(ul,	   2-1,	1-2).
layout(ol,	   2-1,	1-2).
layout(form,	   2-1,	1-2).
layout(frameset,   2-1,	1-2).

layout(head,	   1-1,	1-1).
layout(body,	   1-1,	1-1).
layout(script,	   1-1,	1-1).
layout(select,	   1-1,	1-1).
layout(map,	   1-1,	1-1).
layout(html,	   1-1,	1-1).

layout(tr,	   1-0,	0-1).
layout(option,	   1-0,	0-1).
layout(li,	   1-0,	0-1).
layout(dt,	   1-0,	-).
layout(dd,	   0-0,	-).
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
layout(frame,	   1-1, empty).
layout(col,	   0-0, empty).
layout(area,	   1-0, empty).
layout(input,	   1-0, empty).
layout(param,	   1-0, empty).

layout(p,	   2-1, -).		% omited close
layout(td,	   0-0, 0-0).

layout(div,	   1-0,	0-1).

		 /*******************************
		 *	     PRINTING		*
		 *******************************/

%%	print_html(+List) is det.
%%	print_html(+Out:stream, +List) is det.
%
%	Print list of atoms and layout instructions.  Currently used layout
%	instructions:
%
%		* nl(N)
%		Use at minimum N newlines here.

print_html(List) :-
	current_output(Out),
	write_html(List, Out).
print_html(Out, List) :-
	(   html_current_option(dialect(xhtml))
	->  stream_property(Out, encoding(Enc)),
	    (	Enc == utf8
	    ->	true
	    ;	print_message(warning, html(wrong_encoding(Out, Enc)))
	    ),
	    xml_header(Hdr),
	    write(Out, Hdr), nl(Out)
	;   true
	),
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

%%	html_print_length(+List, -Len) is det.
%
%	Determine the content length of  a   token  list  produced using
%	html//1. Here is an example on  how   this  is used to output an
%	HTML compatible to HTTP:
%	
%	==
%		phrase(html(DOM), Tokens),
%		html_print_length(Tokens, Len),
%		format('Content-type: text/html; charset=UTF-8~n'),
%		format('Content-length: ~d~n~n', [Len]),
%		print_html(Tokens)
%	==

html_print_length(List, Len) :-
	(   html_current_option(dialect(xhtml))
	->  xml_header(Hdr),
	    atom_length(Hdr, L0),
	    L1 is L0+1			% one for newline
	;   L1 = 0
	),
	html_print_length(List, L1, Len).

html_print_length([], L, L).
html_print_length([nl(N)|T], L0, L) :- !,
	join_nl(T, N, Lines, T1),
	L1 is L0 + Lines,		% assume only \n!
	html_print_length(T1, L1, L).
html_print_length([H|T], L0, L) :-
	atom_length(H, Hlen),
	L1 is L0+Hlen,
	html_print_length(T, L1, L).


%%	reply_html_page(:Head, :Body) is det.
%
%	Provide the complete reply as required  by http_wrapper.pl for a
%	page constructed from Head and   Body. The HTTP =|Content-type|=
%	is provided by html_current_option/1.

reply_html_page(Head, Body) :-
	html_current_option(content_type(Type)),
	phrase(page(Head, Body), HTML),
	format('Content-type: ~w~n~n', [Type]),
	print_html(HTML).


		 /*******************************
		 *	PCE EMACS SUPPORT	*
		 *******************************/

:- multifile
	emacs_prolog_colours:goal_colours/2,
	emacs_prolog_colours:style/2,
	emacs_prolog_colours:identify/2,
	prolog:called_by/2.

emacs_prolog_colours:goal_colours(html(HTML,_,_),
				  built_in-[Colours, classify, classify]) :-
	html_colours(HTML, Colours).
emacs_prolog_colours:goal_colours(page(HTML,_,_),
				  built_in-[Colours, classify, classify]) :-
	html_colours(HTML, Colours).
emacs_prolog_colours:goal_colours(page(Head, Body,_,_),
				  built_in-[HC, BC, classify, classify]) :-
	html_colours(Head, HC),
	html_colours(Body, BC).
emacs_prolog_colours:goal_colours(pagehead(HTML,_,_),
				  built_in-[Colours, classify, classify]) :-
	html_colours(HTML, Colours).
emacs_prolog_colours:goal_colours(pagebody(HTML,_,_),
				  built_in-[Colours, classify, classify]) :-
	html_colours(HTML, Colours).
emacs_prolog_colours:goal_colours(reply_html_page(Head, Body),
				  built_in-[HC, BC]) :-
	html_colours(Head, HC),
	html_colours(Body, BC).


					% TBD: Check with do_expand!
html_colours(Var, classify) :-
	var(Var), !.
html_colours(\List, classify) :-
	is_list(List), !.
html_colours(\_, built_in-[dcg]) :- !.
html_colours(&(Entity), built_in-[entity(Entity)]) :- !.
html_colours(List, built_in-ListColours) :-
	List = [_|_], !,
	list_colours(List, ListColours).
html_colours(Term, TermColours) :-
	compound(Term), !,
	Term =.. [Name|Args],
	(   Args = [One]
	->  TermColours = html(Name)-ArgColours,
	    (   layout(Name, _, empty)
	    ->  attr_colours(One, ArgColours)
	    ;   html_colours(One, Colours),
		ArgColours = [Colours]
	    )
	;   Args = [AList,Content]
	->  TermColours = html(Name)-[AColours, Colours],
	    attr_colours(AList, AColours),
	    html_colours(Content, Colours)
	;   TermColours = error
	).
html_colours(_, classify).

list_colours(Var, classify) :-
	var(Var), !.
list_colours([], []).
list_colours([H0|T0], [H|T]) :- !,
	html_colours(H0, H),
	list_colours(T0, T).
list_colours(Last, Colours) :-		% improper list
	html_colours(Last, Colours).

attr_colours(Var, classify) :-
	var(Var), !.
attr_colours([], classify) :- !.
attr_colours(Term, list-Elements) :-
	Term = [_|_], !,
	attr_list_colours(Term, Elements).
attr_colours(Name=_, built_in-[html_attribute(Name), classify]) :- !.
attr_colours(NS:Term, built_in-[html_xmlns(NS), html_attribute(Name)-[classify]]) :-
	compound(Term),
	Term =.. [Name,_], !.
attr_colours(Term, html_attribute(Name)-[classify]) :-
	compound(Term),
	Term =.. [Name,_], !.
attr_colours(Name, html_attribute(Name)) :-
	atom(Name), !.
attr_colours(_, error).

attr_list_colours(Var, classify) :-
	var(Var), !.
attr_list_colours([], []).
attr_list_colours([H0|T0], [H|T]) :-
	attr_colours(H0, H),
	attr_list_colours(T0, T).


:- op(990, xfx, :=).			% allow compiling without XPCE
:- op(200, fy, @).

emacs_prolog_colours:style(html(_), style(bold := @on,
					  colour := magenta4)).
emacs_prolog_colours:style(entity(_), style(colour := magenta4)).
emacs_prolog_colours:style(html_attribute(_), style(colour := magenta4)).
emacs_prolog_colours:style(html_xmlns(_), style(colour := magenta4)).


emacs_prolog_colours:identify(html(Element), Summary) :-
	format(string(Summary), '~w: SGML element', [Element]).
emacs_prolog_colours:identify(entity(Entity), Summary) :-
	format(string(Summary), '~w: SGML entity', [Entity]).
emacs_prolog_colours:identify(html_attribute(Attr), Summary) :-
	format(string(Summary), '~w: SGML attribute', [Attr]).


%	prolog:called_by(+Goal, -Called)
%	
%	Hook into library(pce_prolog_xref).  Called is a list of callable
%	or callable+N to indicate (DCG) arglist extension.


prolog:called_by(html(HTML,_,_), Called) :-
	phrase(called_by(HTML), Called).
prolog:called_by(page(HTML,_,_), Called) :-
	phrase(called_by(HTML), Called).
prolog:called_by(page(Head,Body,_,_), Called) :-
	phrase(called_by([Head,Body]), Called).
prolog:called_by(pagehead(HTML,_,_), Called) :-
	phrase(called_by(HTML), Called).
prolog:called_by(pagebody(HTML,_,_), Called) :-
	phrase(called_by(HTML), Called).

called_by(Var) -->
	{ var(Var) }, !,
	[].
called_by(\G) --> !,
	(   { is_list(G) }
	->  []
	;   [G+2]
	).
called_by([]) --> !,
	[].
called_by([H|T]) --> !,
	called_by(H),
	called_by(T).
called_by(Term) --> 
	{ compound(Term), !,
	  Term =.. [_|Args]
	},
	called_by(Args).
called_by(_) -->
	[].


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(html(expand_failed(What))) -->
	[ 'Failed to translate to HTML: ~p'-[What] ].
prolog:message(html(wrong_encoding(Stream, Enc))) -->
	[ 'XHTML demands UTF-8 encoding; encoding of ~p is ~w'-[Stream, Enc] ].
