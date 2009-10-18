/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_htmlsrc,
	  [ source_to_html/3,		% +Source, +OutStream, +Options
	    write_source_css/0,		% Create pllisting.css
	    write_source_css/1		% +Stream
	  ]).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(doc_colour).
:- use_module(doc_html).
:- use_module(doc_wiki).
:- use_module(doc_modes).
:- use_module(doc_process).
:- use_module(library('http/html_write')).
:- use_module(library(prolog_xref)).

/** <module> HTML source pretty-printer

This module colourises Prolog  source  using   HTML+CSS  using  the same
cross-reference based technology as used by PceEmacs.

@tbd	Create hyper-links to documentation and definitions.
@author Jan Wielemaker
*/

:- thread_local
	lineno/0,			% print line-no on next output
	nonl/0.				% previous tag implies nl (block level)

%%	source_to_html(+In:filename, +Out, +Options) is det.
%
%	Colourise Prolog source as HTML. The idea is to first create a
%	sequence of fragments and then to apply these to the code.
%
%	@param In	A filename
%	@param Out	Term stream(Stream) or file-name specification

source_to_html(Src, stream(Out), Options) :- !,
	retractall(lineno),		% play safe
	retractall(nonl),		% play safe
	colour_fragments(Src, Fragments),
	open(Src, read, In),
	asserta(user:message_hook(_,_,_), Ref),
	call_cleanup((file_base_name(Src, Base),
		      print_html_head(Out, [title(Base), Options]),
		      html_fragments(Fragments, In, Out, [], State, Options),
		      copy_rest(In, Out, State, State1),
		      pop_state(State1, Out, In)),
		     (erase(Ref),
		      close(In))),
	print_html_footer(Out, Options).
source_to_html(Src, FileSpec, Options) :-
	absolute_file_name(FileSpec, OutFile, [access(write)]),
	open(OutFile, write, Out, [encoding(utf8)]),
	call_cleanup(source_to_html(Src, stream(Out), Options),
		     close(Out)).

%%	print_html_head(+Out:stream, +Options) is det.
%
%	Print the =DOCTYPE= line and HTML header.  Options:
%
%		* header(Bool)
%		Only print the header if Bool is not =false=
%
%		* title(Title)
%		Title of the HTML document
%
%		* stylesheets(List)
%		Reference to the CSS style-sheets.
%
%		* format_comments(Bool)
%		If =true= (default), format structured comments.

print_html_head(Out, Options) :-
	option(header(true), Options, true), !,
	option(title(Title), Options, 'Prolog source'),
	option(stylesheets(Sheets), Options, ['pllisting.css', 'pldoc.css']),
	format(Out,
	       '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" \
	       "http://www.w3.org/TR/html4/strict.dtd">~n~n', []),
	format(Out, '<html>~n', []),
	format(Out, '  <head>~n', []),
	format(Out, '    <title>~w</title>~n', [Title]),
	forall(member(Sheet, Sheets),
	       format(Out, '    <link rel="stylesheet" type="text/css" href="~w">~n', [Sheet])),
	format(Out, '  </head>~n', []),
	format(Out, '<body>~n', []).
print_html_head(_, _).

print_html_footer(Out, Options) :-
	option(header(true), Options, true), !,
	format(Out, '~N</body>~n', []),
	format(Out, '</html>', []).
print_html_footer(_, _).


%%	html_fragments(+Fragments, +In, +Out, +State, +Options) is det.
%
%	Copy In to Out, inserting HTML elements using Fragments.

html_fragments([], _, _, State, State, _).
html_fragments([H|T], In, Out, State0, State, Options) :-
	html_fragment(H, In, Out, State0, State1, Options),
	html_fragments(T, In, Out, State1, State, Options).

%%	html_fragment(+Fragment, +In, +Out,
%%		      +StateIn, -StateOut, +Options) is det.
%
%	Print from current position upto the end of Fragment.  First
%	clause deals with structured comments.

html_fragment(fragment(Start, End, structured_comment, []),
	      In, Out, State0, [], Options) :-
	option(format_comments(true), Options, true), !,
	copy_without_trailing_white_lines(In, Start, Out, State0, State1),
	pop_state(State1, Out, In),
	Len is End - Start,
	read_n_codes(In, Len, Comment),
	is_structured_comment(Comment, Prefix),
	indented_lines(Comment, Prefix, Lines0),
	(   section_comment_header(Lines0, Header, Lines1)
	->  wiki_lines_to_dom(Lines1, [], DOM),
	    phrase(pldoc_html:html(div(class(comment),
				       [Header|DOM])), Tokens),
	    print_html(Out, Tokens)
	;   stream_property(In, file_name(File)),
	    line_count(In, Line),
	    (	xref_module(File, Module)
	    ->	true
	    ;	Module = user
	    ),
	    process_modes(Lines0, Module, File:Line, Modes, Args, Lines1),
	    DOM = [\pred_dt(Modes, pubdef, []), dd(class=defbody, DOM1)],
	    wiki_lines_to_dom(Lines1, Args, DOM0),
	    strip_leading_par(DOM0, DOM1),
	    phrase(pldoc_html:html(DOM), Tokens),		% HACK
	    format(Out, '<dl class="comment">~n', [Out]),
	    print_html(Out, Tokens),
	    format(Out, '</dl>~n', [Out])
	).
html_fragment(fragment(Start, End, Class, Sub),
	      In, Out, State0, State, Options) :-
	copy_to(In, Start, Out, State0, State1),
	start_fragment(Class, Out, State1, State2),
	html_fragments(Sub, In, Out, State2, State3, Options),
	copy_to(In, End, Out, State3, State4),	% TBD: pop-to?
	end_fragment(Out, In, State4, State).

start_fragment(Class, Out, State, [Push|State]) :-
	element(Class, Tag, CSSClass), !,
	Push =.. [Tag,class(CSSClass)],
	format(Out, '<~w class="~w">', [Tag, CSSClass]).
start_fragment(Class, Out, State, [span(class(SpanClass))|State]) :-
	functor(Class, SpanClass, _),
	format(Out, '<span class="~w">', [SpanClass]).

end_fragment(Out, In, [span(class(directive))|State], State) :- !,
	format(Out, '</span>', []),
	(   peek_code(In, 10),
	    \+ nonl
	->  assert(nonl)
	;   true
	).
end_fragment(Out, _, [Open|State], State) :-
	retractall(nonl),
	functor(Open, Element, _),
	format(Out, '</~w>', [Element]).

pop_state([], _, _) :- !.
pop_state(State, Out, In) :-
	end_fragment(Out, In, State, State1),
	pop_state(State1, Out, In).


%%	copy_to(+In:stream, +End:int, +Out:stream, +State) is det.
%
%	Copy data from In to Out   upto  character-position End. Inserts
%	HTML entities for HTML the reserved characters =|<&>|=. If State
%	does not include a =pre= environment,   create  one and skip all
%	leading blank lines.

copy_to(In, End, Out, State, State) :-
	member(pre(_), State), !,
	copy_to(In, End, Out).
copy_to(In, End, Out, State, [pre(class(listing))|State]) :-
	format(Out, '<pre class="listing">~n', [Out]),
	line_count(In, Line0),
	read_to(In, End, Codes0),
	delete_leading_white_lines(Codes0, Codes, Line0, Line),
	assert(lineno),
	write_codes(Codes, Line, Out).

%%	delete_leading_white_lines(+CodesIn, -CodesOut, +LineIn, -Line) is det.
%
%	Delete leading white lines. Used  after structured comments. The
%	last two arguments update the  start-line   number  of the <pre>
%	block that is normally created.

delete_leading_white_lines(Codes0, Codes, Line0, Line) :-
	append(LineCodes, [10|Rest], Codes0),
	all_spaces(LineCodes), !,
	Line1 is Line0 + 1,
	delete_leading_white_lines(Rest, Codes, Line1, Line).
delete_leading_white_lines(Codes, Codes, Line, Line).

%%	copy_without_trailing_white_lines(+In, +End, +StateIn, -StateOut) is det.
%
%	Copy input, but skip trailing white-lines. Used to copy the text
%	leading to a structured comment.

copy_without_trailing_white_lines(In, End, Out, State, State) :-
	member(pre(_), State), !,
	line_count(In, Line),
	read_to(In, End, Codes0),
	delete_trailing_white_lines(Codes0, Codes),
	write_codes(Codes, Line, Out).
copy_without_trailing_white_lines(In, End, Out, State0, State) :-
	copy_to(In, End, Out, State0, State).

delete_trailing_white_lines(Codes0, []) :-
	all_spaces(Codes0), !.
delete_trailing_white_lines(Codes0, Codes) :-
	append(Codes, Tail, [10|Rest], Codes0), !,
	delete_trailing_white_lines(Rest, Tail).
delete_trailing_white_lines(Codes, Codes).

%%	append(-First, -FirstTail, ?Rest, +List) is nondet.
%
%	Split List.  First part is the difference-list First-FirstTail.

append(T, T, L, L).
append([H|T0], Tail, L, [H|T]) :-
	append(T0, Tail, L, T).

all_spaces([]).
all_spaces([H|T]) :-
	code_type(H, space),
	all_spaces(T).

copy_to(In, End, Out) :-
	line_count(In, Line),
	read_to(In, End, Codes),
	write_codes(Codes, Line, Out).

read_to(In, End, Codes) :-
	character_count(In, Here),
	Len is End - Here,
	read_n_codes(In, Len, Codes).

%%	write_codes(+Codes, +Line, +Out) is det.
%
%	Write codes that have been read starting at Line.

write_codes([], _, _).
write_codes([H|T], L0, Out) :-
	content_escape(H, Out, L0, L1),
	write_codes(T, L1, Out).

%%	content_escape(+Code, +Out, +Line0, -Line) is det
%
%	Write Code to Out, while taking care of.
%
%		* Use HTML entities for =|<&>|=
%		* If a line-no-tag is requested, write it
%		* On \n, post a line-no request.  If nonl/0 is set,
%		  do _not_ emit a newline as it is implied by the
%		  closed environment.

content_escape(_, Out, L, _) :-
	retract(lineno),
	write_line_no(L, Out),
	fail.
content_escape(0'\n, Out, L0, L) :- !,
	L is L0 + 1,
	(   retract(nonl)
	->  true
	;   nl(Out)
	),
	assert(lineno).
content_escape(0'<, Out, L, L) :- !,
	format(Out, '&lt;', []).
content_escape(0'>, Out, L, L) :- !,
	format(Out, '&gt;', []).
content_escape(0'&, Out, L, L) :- !,
	format(Out, '&amp;', []).
content_escape(C, Out, L, L) :-
	put_code(Out, C).

write_line_no(LineNo, Out) :-
	format(Out, '<span class="line-no">~|~t~d~4+</span>', [LineNo]).

%%	copy_rest(+In, +Out, +StateIn, -StateOut) is det.
%
%	Copy upto the end of the input In.

copy_rest(In, Out, State0, State) :-
	copy_to(In, -1, Out, State0, State).

%%	read_n_codes(+In, +N, -Codes)
%
%	Read the next N codes from In as a list of codes. If N < 0, read
%	upto the end of stream In.

read_n_codes(_, 0, []) :- !.
read_n_codes(In, N, Codes) :-
	get_code(In, C0),
	read_n_codes(N, C0, In, Codes).

read_n_codes(1, C, _, [C]) :- !.
read_n_codes(_, -1, _, []) :- !.
read_n_codes(N, C, In, [C|T]) :-
	get_code(In, C2),
	N2 is N - 1,
	read_n_codes(N2, C2, In, T).


%%	element(+Class, -HTMLElement, -CSSClass) is nondet.
%
%	Map classified objects to an  HTML   element  and CSS class. The
%	actual  clauses  are  created   from    the   1st   argument  of
%	prolog_src_style/2.

term_expansion(element/3, Clauses) :-
	findall(C, element_clause(C), Clauses).

%element_tag(directive, div) :- !.
element_tag(_, span).

element_clause(element(Term, Tag, CSS)) :-
	span_term(Term, CSS),
	element_tag(Term, Tag).

span_term(Classification, Class) :-
	prolog_src_style(Classification, _Style),
	css_class(Classification, Class).

css_class(Class, Class) :-
	atom(Class), !.
css_class(Term, Class) :-
	Term =.. [P1,A|_],
	(   var(A)
	->  Class = P1
	;   css_class(A, P2),
	    atomic_list_concat([P1, -, P2], Class)
	).

element/3.

%%	write_source_css is det.
%%	write_source_css(+Out:stream) is det.
%
%	Create   a   style-sheet   from    the   style-declarations   in
%	doc_colour.pl    and    the    element     declaration    above.
%	write_source_css/0 writes the style-sheet to =|pllisting.css|=.

:- op(990, xfx, :=).

write_source_css :-
	open('pllisting.css', write, Out),
	call_cleanup(write_source_css(Out),
		     close(Out)).

write_source_css(Out) :-
	(   prolog_src_style(Term, Style0),
	    (	html_style(Term, Style)
	    ->	true
	    ;	Style = Style0
	    ),
	    element(Term2, Tag, Class),
	    Term2 =@= Term,
	    findall(Name=Value, style_attr(Style, Name, Value),
		    [N=V|NV]),
	    format(Out, '~w.~w~n', [Tag, Class]),
	    format(Out, '{ ~w: ~w;~n', [N, V]),
	    forall(member(N2=V2, NV),
		   format(Out, '  ~w: ~w;~n', [N2, V2])),
	    format(Out, '}~n~n', []),
	    fail
	;   true
	).

style_attr(Style, Name, Value) :-
	arg(_, Style, PceName := PceValue),
	pce_to_css_attr(PceName, Name),
	pce_to_css_value(Name, PceValue, Value).

pce_to_css_attr(colour, color).
pce_to_css_attr(background, 'background-color').
pce_to_css_attr(underline, 'text-decoration').
pce_to_css_attr(bold, 'font-weight').
pce_to_css_attr('font-style', 'font-style').
pce_to_css_attr(display, display).

pce_to_css_value(color, Name, RGB) :-
	x11_colour_name_to_rgb(Name, RGB).
pce_to_css_value('background-color', Name, RGB) :-
	x11_colour_name_to_rgb(Name, RGB).
pce_to_css_value('text-decoration', @(on), underline).
pce_to_css_value('font-weight', @(on), bold).
pce_to_css_value('font-style', Style, Style).

x11_colour_name_to_rgb(red, red) :- !.
x11_colour_name_to_rgb(blue, blue) :- !.
x11_colour_name_to_rgb(Name, RGB) :-
	get(@(pce), convert, Name, colour, Obj),
	get(Obj, red, R),
	get(Obj, green, G),
	get(Obj, blue, B),
	R256 is R//256,
	G256 is G//256,
	B256 is B//256,
	format(atom(RGB),
	       '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+',
	       [R256, G256, B256]).

%%	html_style(+Term, -Style) is semidet.
%
%	Redefine styles from prolog_src_style/2 for better ones on
%	HTML output.

html_style(var,
	   style(colour := red4,
		 'font-style' := italic)).
html_style(directive,
	   style(background := grey90,
		 'display' := block)).

