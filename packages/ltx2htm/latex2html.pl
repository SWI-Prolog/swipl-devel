/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

:- module(tex,
	  [ welcome/0,
	    latex2html/1,		% +BaseName
	    macro_expand/2,		% +In, -Out
	    translate/4,		% +In, +ModeIn, -ModeOut, -Out
	    translate/3,		% +In, +ModeIn, -Out
	    latex2html_module/0,	% Register extension module
	    tex_input_directory/1,	% +Input Dir
	    step_counter/2,		% +Name, -NewValue
	    ps2gif/3,			% +PsFile, -GifFile, +Options
	    translate_command/4,	% +In, +ModeIn, -ModeOut, -HTML
	    translate_environment/4,	% +In, +ModeIn, -ModeOut, -HTML
	    translate_reference/4,	% +Kind, +RefPrefix, +Label, -HTML
	    translate_footnote/2,	% +Text, -HTML
	    translate_table/3,		% +Format, +BodyTokens, -HTML
	    translate_section/4,	% +Level, -/*, +Title, -HTML
	    translate_section/5,	% +Level, -/*, +Title, -HTML, +FileBase
	    current_setting/1,		% +Type(-Value ...)
	    do_float/2,			% +Float(+Number), :Goal
	    tex_load_commands/1,	% +BaseName
	    add_to_index/1,		% +Term
	    add_to_index/2,		% +Term, +Tag
	    clean_tt/2			% +Raw, -Clean
	  ]).
:- use_module(library(quintus)).

version('0.96').			% for SWI-Prolog 5.0.6
page_header('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n\n').

:- dynamic			
	html_output_dir/1,		% output relative to this dir
	tex_file_base/1,		% Basename of the main file
	html_file_base/1,		% Basename of main output file
	html_split_level/1,		% Split upto this level
	bodycolor/1,			% \bodycolor storage
	title/1,			% \title{} storage
	author/1,			% \auther{} command storage
	link_image/2.			% Id, Image
:- discontiguous
	cmd/2,
	cmd/3,
	cmd/4.


html_split_level(2).
html_file_base('Title').

:- multifile
	user:file_search_path/2.
:- dynamic
	user:file_search_path/2.

user:file_search_path(foreign, library(Lib)) :-
	current_prolog_flag(arch, Arch),
	atom_concat('lib/', Arch, Lib).
user:file_search_path(psfig, tex(figs)).
user:file_search_path(includegraphics, tex(figs)).
user:file_search_path(tex, '.').
user:file_search_path(img, '.').
user:file_search_path(img, icons).
user:file_search_path(img, library(icons)).

:- load_foreign_library(user:foreign(tex)).

:- op(100, fx, user:(#)).

		 /*******************************
		 *         TEX INPUTS		*
		 *******************************/

read_tex_inputs :-
	getenv('TEXINPUTS', Val), !,
	split(Val, ":", PathElement),
	retractall(user:file_search_path(tex, _)),
	reverse(PathElement, RevPath),
	forall(member(E, RevPath), assert_tex_input(E)).
read_tex_inputs.

assert_tex_input('') :- !,
	asserta(user:file_search_path(tex, '.')).
assert_tex_input(Dir) :-
	asserta(user:file_search_path(tex, Dir)).

		 /*******************************
		 *       EXTENSION MODULES	*
		 *******************************/

:- dynamic
	tex_extension_module/1.

tex_extension_module(tex).

:- module_transparent
	latex2html_module/0.

latex2html_module :-
	context_module(M),
	(   tex:tex_extension_module(M)
	->  true
	;   asserta(tex:tex_extension_module(M))
	),
	M:dynamic((cmd/2, cmd/3, cmd/4, env/2, (#)/2)),
	M:discontiguous((cmd/2, cmd/3, cmd/4, env/2, (#)/2)).

%	Load a tex command file

tex_load_commands(File) :-
	(   member(Term, [tex(File), library(File)]),
	    absolute_file_name(Term,
			       [ extensions([cmd]),
				 access(read),
				 file_errors(fail)
			       ],
			       CmdFile)
	->  tex_read_commands(CmdFile),
	    format(user_error, 'Loaded LaTeX commands from "~w"~n', [CmdFile])
	;   format(user_error, 'Can not find command file "~w"~n', [File]),
	    fail
	).

tex_input_directory(D) :-
	asserta(user:file_search_path(tex, D)).

%	current_setting(?Term)
%
%	Report current settings to the extension module

current_setting(keep_figures) :-
	keep_figures(true).
current_setting(html_output_dir(Dir)) :-
	html_output_dir(Dir).


		 /*******************************
		 *	      SETTINGS		*
		 *******************************/

:- dynamic
	keep_figures/1,
	onefile/1,
	makeindex/1,
	title/1,
	auther/1.

keep_figures(true).
onefile(false).
makeindex(false).
title('No title').
author('Anonymous').


		 /*******************************
		 *	      TOPLEVEL		*
		 *******************************/


run_latex2html(TeXFile) :-
	reset_footnotes,
	reset_counters,
	reset_sections,
	reset_index,
	reset_labels,
	reset_cites,
	reset_output,
	tex_tokens(TeXFile, TeXTokens),
	(   member(env(document, _, _), TeXTokens)
	->  translate(TeXTokens, preamble, HTML)
	;   format(user_error,
		   'No document environment; processing anyway~n', []),
	    translate(TeXTokens, document, HTML)
	),
	once(html_file_base(Base)),
	expand_macros([tell(Base), #header], Header),
	footnote_html(FootNotes),
	make_index(Index),
	flatten([ Header,
		  HTML,
		  FootNotes,
		  Index
		],
		HtmlDocument),
	collect_labels(HtmlDocument, Base),
	write_html(HtmlDocument),
	close_output.
	

latex2html(Spec) :-
	welcome,
	tex_load_commands(latex),
	file_name_extension(Spec, tex,  TeXFile),
	file_name_extension(Base, tex,  TeXFile),
	absolute_file_name(tex(TeXFile),
			   [ access(read)
			   ],
			   TheTeXFile),
	asserta(html_output_dir(Base)),
	asserta(tex_file_base(Base)),
	run_latex2html(TheTeXFile),
	goodbye,
	retract(tex_file_base(_)),
	retract(html_output_dir(_)).

%	make_output_directory
%
%	Create the output directory.

:- dynamic
	done_make_output_directory/0.

make_output_directory :-
	done_make_output_directory, !.
make_output_directory :-
	html_output_dir(Dir), !,
	(   exists_directory(Dir)
	->  true
	;   sformat(Cmd, 'mkdir ~w~n', [Dir]),
	    shell(Cmd)
	),
	assert(done_make_output_directory).

:- dynamic
	current_html_output_db/1,	% Base
	in_head/1.

reset_output :-
	retractall(current_html_output_db(_)),
	retractall(in_head(_)).

open_output(Base) :-
	make_output_directory,
	html_output_dir(Dir), !,
	concat_atom([Dir, /, Base, '.html'], HtmlFile),
	tex_tell(HtmlFile),
	page_header(Header),
	put_html_token(html(Header)),
	put_html_token(html('<HTML>')),
	asserta(current_html_output_db(Base)).

close_output :-
	(   retract(current_html_output_db(_))
	->  put_html_token(html('</BODY>')),
	    put_html_token(html('</HTML>'))
	;   true
	),
	tex_told.

current_html_output(Raw) :-
	current_html_output_db(File), !,
	File = Raw.

welcome :-
	version(Version),
	format(user_error, 'Welcome to LaTeX2HTML version ~w~n~n', [Version]).

goodbye :-
	html_output_dir(Dir),
	html_file_base(Html),
	format(user_error, '~*t~72|~n', [0'*]),
	format(user_error,
	       'Translation completed; output written to "~w/~w.html".~n',
	       [Dir, Html]),
	format(user_error, 'Prolog statistics:~n~n', []),
	statistics.

%	translate(+TeX, +Mode, -HTML)

translate(TeX, Mode, HTML) :-
	translate(TeX, Mode, _, HTML).

translate([], Mode, Mode, []) :- !.
translate([\(Section, -, [{Title}])|T], Mode, Mode, HTML) :-
	trans_section(Section, Title, T, TitleHtml), !,
	append(TitleHtml, BodyHtml, HTML),
	translate(T, Mode, Mode, BodyHtml).
translate([H0|T0], Mode0, Mode, [H|T]) :- !,
	translate_1(H0, Mode0, Mode1, H),
	translate(T0, Mode1, Mode, T).
translate(X, Mode, Mode, []) :-
	format(user_error, 'translate(~p) failed in mode "~w"~n', [X, Mode]).

translate_1(Term, Mode0, Mode, HTML) :-
	(   translate_2(Term, Mode0, Mode, HTML)
	->  true
	;   format(user_error, 'translate_2(~p, ~w, ...) failed~n',
		   [Term, Mode0]),
	    Mode = Mode0,
	    HTML = []
	).

translate_2(\(Cmd, Mod, Args), Mode0, Mode, HTML) :-	% \cmd*{Args}
	Term =.. [Cmd, Mod|Args],
	translate_command(Term, Mode0, Mode, HTML).
translate_2(\(Cmd, Args), Mode0, Mode, HTML) :-		% \cmd{Args}
	Term =.. [Cmd|Args],
	translate_command(Term, Mode0, Mode, HTML).
translate_2(\Cmd, Mode0, Mode, HTML) :-			% \cmd
	translate_command(Cmd, Mode0, Mode, HTML).
translate_2(env(Env, Args, Bdy), Mode0, Mode, HTML) :-	% \begin{x} ... \end{x}
	Term =.. [Env, Args, Bdy],
	translate_environment(Term, Mode0, Mode, HTML).
translate_2($$(Expr), Mode, Mode, #quote(#var(HTML))) :-% $$...$$
	tex_atom_to_tokens(Expr, Tokens),
	translate(Tokens, math, _, HTML).
translate_2($(Expr), Mode, Mode, #var(HTML)) :-		% $...$
	tex_atom_to_tokens(Expr, Tokens),
	translate(Tokens, math, _, HTML).
translate_2(verbatim(_Cmd, Text), Mode, Mode, 		% \begin{verbatim} ...
	    #listing(#pre(pre(Text)))).
translate_2(verb(_, Text), pcecode, pcecode, pre(Text)).
translate_2(Layout, pcecode, pcecode, []) :-
	atomic(Layout), !.
translate_2('\n', Mode, Mode, [html('<BR>')]) :-
	Mode = group(Atts),
	memberchk(obeylines, Atts), !.
translate_2(verb(_, Text), Mode, Mode, #code(Text)).	% \verbX...X
translate_2([Atom], Mode, Mode, nospace(Atom)) :-	% {foo} 
	atomic(Atom).
translate_2(Group, Mode, Mode, HTML) :-			% {...}
	Group = [_|_],
	translate_group(Group, HTML).
translate_2(~, Mode, Mode, html('&nbsp;')).		% ~
translate_2(Atom0, Mode, Mode, Atom) :-			% Normal word
	atomic(Atom0),
	(   Mode = group(Atts),
	    memberchk(font(sc), Atts)
	->  upcase_atom(Atom0, Atom)
	;   Atom = Atom0
	).

translate_group(Group, [HTML, Close]) :-		% {...}
	translate(Group, group([]), MEnd, HTML),
	(   MEnd = group(Attributes),
	    member(font(EndFont), Attributes),
	    html_font(EndFont, _, Close)
	->  true
	;   Close = []
	).

translate_command(Cmd, Mode0, Mode, HTML) :-
	translate_cmd(Cmd, Mode0, Mode, HTML0),
	expand_macros(HTML0, HTML).

translate_cmd(Cmd, Mode0, Mode, HTML) :-
	tex_extension_module(M),
	M:cmd(Cmd, Mode0, Mode, HTML), !.
translate_cmd(Cmd, Mode, Mode, HTML) :-
	tex_extension_module(M),
	M:cmd(Cmd, Mode, HTML), !.
translate_cmd(Cmd, Mode, Mode, HTML) :-
	tex_extension_module(M),
	M:cmd(Cmd, HTML), !.
translate_cmd(Cmd, Mode, Mode, []) :-
	functor(Cmd, Name, _),
	format(user_error,
	       'Failed to translate \\~w in mode "~w"~n', [Name, Mode]),
	format(user_error, 'Term: "~p"~n', [Cmd]),
	true.	

translate_environment(Env, Mode, Mode, HTML) :-
	translate_env(Env, Mode, Mode, HTML0),
	expand_macros(HTML0, HTML).

translate_env(Env, Mode, Mode, HTML) :-
	tex_extension_module(M),
	M:env(Env, HTML), !.
translate_env(Env, Mode, Mode, []) :-
	functor(Env, Name, _),
	format(user_error,
	       'Failed to translate \\begin{~w} ... \\end{~w}~n',
	       [Name, Name]),
%	format(user_error, 'Term: "~p"~n', [Env]),
	true.


		 /*******************************
		 *	       LANGUAGE		*
		 *******************************/

language_map(figure,	'Figure').
language_map(table,	'Table').


		 /*******************************
		 *	      # MACROS		*
		 *******************************/

:- dynamic
	in_anchor/0.			% avoid nesting anchors

%	#(+Macro, -Expansion)
%	Do HTML macro expansion on the fly.

#(tell(_File),		[]) :- onefile(true).
#(tell(File),		tell(File)).
#(head(Head),		[html('<HEAD>'), HtmlHead, html('</HEAD>')]) :-
	expand_macros(Head, HtmlHead).
#(beginbody,		html(Body)) :-
	bodycolor(Colour), !,
	sformat(Body, '<BODY BGCOLOR="~w">', [Colour]).
#(beginbody,		html('<BODY>')).
#(endbody,		html('</BODY>')).
#(thetitle,		Title) :-
	title(Title).
#(theauthor,		Author) :-
	author(Author).
#(nameof(Type),		Name) :-
	language_map(Type, Name).
#(title(Text),		[html('<TITLE>'),  Text, html('</TITLE>')]).
#(var(Text),		[html('<VAR>'),    Text, html('</VAR>')]).
#(code(Text),		[html('<CODE>'),   Text, html('</CODE>')]).
#(pre(Text),		[html('<PRE>'),    Text, html('</PRE>')]).
#(xmp(Text),		[html('<XMP>'),    Text, html('</XMP>')]).
#(strong(Text),		[html('<STRONG>'), Text, html('</STRONG>')]).
#(em(Text),		[html('<EM>'),     Text, html('</EM>')]).
#(b(Text),		[html('<B>'),      Text, html('</B>')]).
#(i(Text),		[html('<I>'),      Text, html('</I>')]).
#(tt(Text),		[html('<TT>'),	   Text, html('</TT>')]).
#(sc(Text),		[html('<font size=-1>'), % TBD: upcase Text
					   Text, html('</font>')]).
#(center(Text),		[html('<CENTER>'), Text, html('</CENTER>')]).
#(right(Text),		[html('<RIGHT>'),  Text, html('</RIGHT>')]).
#(quote(Text),		[html('<BLOCKQUOTE>'), Text, html('</BLOCKQUOTE>')]).
#(listing(Text),	[html('<P><TABLE WIDTH="90%" ALIGN=center BORDER=6 BGCOLOR="#e0e0e0"><TR><TD NOWRAP>'), Text,
			 html('</TABLE>')]).
#(abstract(Text),	[html('<CENTER><H3>Abstract</H3></Center>'),
			 html('<TABLE WIDTH="90%" ALIGN=center BORDER=2 BGCOLOR="#f0f0f0"><TR><TD>'), Text,
			 html('</TABLE>')]).
#(embrace([O,C],Text),	[nospace(OA),	   Text, nospace(CA)]) :-
	char_code(OA, O),
	char_code(CA, C).
#(embrace(Text),	#embrace("()", Text)).
#(h(Level, Title),	[html(OpenH), Title, html(CloseH)]) :-
	h(Level, OpenH, CloseH).
#(predref(RN, Arity),   #lref(Text, Text)) :-
	clean_tt(RN, Name),
	sformat(Text, '~w/~w', [Name, Arity]).
#(row(Columns),		[html('<TR>'), HtmlCols, html('</TR>')]) :-
	add_td(Columns, HtmlCols).
#(label(Lbl, Text, Tag),label(ALabel, Expanded, Tag)) :-
	string_to_atom(Lbl, ALabel),
	expand_macros(Text, Expanded).
#(label(Lbl, Text),	label(ALabel, Expanded, -)) :-
	string_to_atom(Lbl, ALabel),
	expand_macros(Text, Expanded).
#(lref(_Label, Text),	Text) :-
	in_anchor, !.
#(lref(Label, Text),	lref(ALabel, Expanded)) :-
	canonise_label(Label, ALabel),
	asserta(in_anchor),
	expand_macros(Text, Expanded),
	retractall(in_anchor).
#(iflref(Label, Text),	iflref(ALabel, Expanded)) :-
	canonise_label(Label, ALabel),
	expand_macros(Text, Expanded).
#(lback(Label, Text),	lback(ALabel, Expanded)) :-
	string_to_atom(Label, ALabel),
	expand_macros(Text, Expanded).
#(lforw(Label, Text),	lforw(ALabel, Expanded)) :-
	string_to_atom(Label, ALabel),
	expand_macros(Text, Expanded).
#(url(_URL, Text),	Text) :-
	in_anchor, !.
#(url(URL, Text),	[html(Anchor), Expanded, html('</A>')]) :-
	sformat(Anchor, '<A HREF="~w">', URL),
	asserta(in_anchor),
	expand_macros(Text, Expanded),
	retractall(in_anchor).
#(cite(Key),		[ html('<CITE>'),
			  Cites,
			  html('</CITE>')
			]) :-
	cite_references(Key, cite, Cites).
#(opencite(Key),	[ html('<CITE>'),
			  Cites,
			  html('</CITE>')
			]) :-
	cite_references(Key, cite, Cites).
#(yearcite(Key),	[ html('<CITE>'),
			  Cites,
			  html('</CITE>')
			]) :-
	cite_references(Key, yearcite, Cites).
#(header,		HTML) :-
	node_header(HTML).
#(header(Tag),		HTML) :-
	node_header(Tag, HTML).
#(footer(Tag),		HTML) :-
	node_footer(Tag, HTML).
#(next_and_prev_references,
        [ #iflref(prevfile,  	    '[Previous]'), ' ',
	  #iflref(nextfile,  	    '[Next]')
	]).
#(home_reference,
 	[ #iflref(home,  	    '[Home]')
	]).
#(Macro, []) :-
	format(user_error,
	       'Post-processing macro #~p could not be expanded~n',
	       [Macro]),
	gtrace, fail.
	 
add_td([], []).
add_td([H|T0], [html('<TD>'), H, html('</TD>')|T]) :-
	add_td(T0, T).

cite_references(KeyIn, Functor, Refs) :-
	split(KeyIn, ",", Keys),
	make_cite_references(Keys, Functor, Refs).

make_cite_references([], _, []).
make_cite_references([Key],    F, [#lref(Key, Term)]) :- !,
	Term =.. [F, Key].
make_cite_references([Key|T0], F, [#lref(Key, Term), nospace(','), ' '|T]) :-
	Term =.. [F, Key],
	make_cite_references(T0, F, T).

%	canonise_label(+Raw, -Canonical)
%
%	Ensures the label is either an atom, or fileof(Atom), so unification
%	will work properly.

canonise_label(Atom, Atom) :-
	atom(Atom), !.
canonise_label(Atomic, Atom) :-
	atomic(Atomic), !,
	string_to_atom(Atomic, Atom).
canonise_label(fileof(In), Out) :-
	onefile(true), !,
	canonise_label(In, Out).
canonise_label(fileof(In), fileof(Out)) :-
	canonise_label(In, Out).


%	expand_macros(Raw, Expanded).
%	macro_expand(Raw, Expanded).
%
%	Expand +Term, Mode+Term and #Macro commands into plain HTML

macro_expand(In, Out) :-
	expand_macros(In, Out).

expand_macros([], []) :- !.
expand_macros([[]|T0], [T]) :- !,
	expand_macros(T0, T).
expand_macros([H0|T0], [H|T]) :- !,
	expand_macro(H0, H),
	expand_macros(T0, T).
expand_macros(Macro, Expanded) :-
	expand_macro(Macro, Expanded).

expand_macro(+Term, HTML) :- !,
	translate(Term, normal, HTML0),
	expand_macros(HTML0, HTML).
expand_macro(Mode+Term, HTML) :- !,
	translate(Term, Mode, HTML0),
	expand_macros(HTML0, HTML).
expand_macro(#Macro, HTML) :-
	tex_extension_module(M),
	M:'#'(Macro, HTML0), !,
	expand_macros(HTML0, HTML).
expand_macro(List, Expanded) :-
	List = [_|_], !,
	expand_macros(List, Expanded).
expand_macro(NoExpand, NoExpand).


		 /*******************************
		 *	       CITES		*
		 *******************************/

:- dynamic
	cite/2.				% Key, Cite (HTML)

reset_cites :-
	retractall(cite(_, _)).

		 /*******************************
		 *	       FLOAT		*
		 *******************************/

:- dynamic
	current_float/2.		% Type, Number

:- meta_predicate
	do_float(+, :).

%	do_float(+Type(+Number), +Goal)
%
%	Run goal, registering the current float object processed.

do_float(Term, Goal) :-
	functor(Term, Type, 1),
	arg(1, Term, Number),
	asserta(tex:current_float(Type, Number), Ref),
	ignore(Goal),
	erase(Ref).


		 /*******************************
		 *	      LABEL/REF		*
		 *******************************/

:- dynamic
	label/3,			% Label, File, Index
	next_file/2.			% File, NextFile

reset_labels :-
	retractall(label(_, _, _)),
	retractall(next_file(_, _)).

collect_labels([], _) :- !.
collect_labels([tell(File)|T], PreviousFile) :- !,
	(   PreviousFile \== File
	->  assert(next_file(PreviousFile, File))
	;   true
	),
	collect_labels(T, File).
collect_labels([label(Label, _, Tag)|T], File) :- !,
	assert(label(Label, File, Tag)),
	collect_labels(T, File).
collect_labels([lback(Label, _)|T], File) :- !,
	assert(label(Label, File, -)),
	collect_labels(T, File).
collect_labels([lforw(Label, _)|T], File) :- !,
	concat('back-to-', Label, BackLabel),
	assert(label(BackLabel, File, -)),
	collect_labels(T, File).
collect_labels([_|T], File) :-
	collect_labels(T, File).

label_tag(Tag) :-
	current_float(_, Tag), !.
label_tag(Tag) :-
	section_tag(Tag).


		 /*******************************
		 * 	    ENVIRONMENTS	*
		 *******************************/

%	env(+Env, -HTML)
%
%	Translate an environment.

env(pcecode(_, Tokens), #listing(#pre(HTML))) :-
	translate(Tokens, pcecode, HTML).
env(summarylist(_, Summary),
    [ html('<TABLE>'),
      +Summary,
      html('</TABLE>')
    ]).
env(comment(_, _), []).
env(htmlonly(_, Tokens), HTML) :-
	translate(Tokens, normal, HTML).
env(document(_, Contents), HTML) :- !,
	translate(Contents, document, HTML).
env(quote(_, Tokens), #quote(Quote)) :-
	translate_group(Tokens, Quote).
env(abstract(_, Tokens), #abstract(Quote)) :-
	translate(Tokens, normal, Quote).
env(center(_, Tokens), #center(Center)) :-
	translate(Tokens, normal, Center).
env(titlepage(_, _Page), []) :-
	format(user_error, 'Ignored the titlepage~n', []).
env(tabular([{Format}], Tokens), HTML) :-
	translate_table(Format, Tokens, HTML).
env(array([{Format}], Tokens), HTML) :-
	translate_table(Format, Tokens, HTML).
env(table(_, Tokens), Table) :-
	step_counter(table, Tab),
	asserta(current_float(table, Tab), Ref),
	translate(Tokens, normal, Table),
	erase(Ref).
env(figure(_, Tokens), Figure) :-
	step_counter(figure, Fig),
	asserta(current_float(figure, Fig), Ref),
	translate(Tokens, normal, Figure),
	erase(Ref).
env(minipage(_, Tokens), Page) :-
	translate(Tokens, normal, Page).
env(thebibliography(Args, Tokens),
    [ SectionHeader,
      Open,
      HtmlItems,
      Close
    ]) :- !,
	translate_section(1, -, ['Bibliography'], SectionHeader,
			  'Bibliography'),
	(   list_command(List, Args, Open, Close),
	    items(Tokens, Items),
	    translate_items(Items, List, HtmlItems)
	->  true
	;   format(user_error, 'Failed to translate "~w" list~n', [List])
	).
env(Env, [Open, HtmlItems, Close]) :-		% General lists
	functor(Env, List, _),
	tex_environment_function(List, list), !,
	arg(1, Env, Args),
	arg(2, Env, Tokens),
	(   list_command(List, Args, Open, Close),
	    items(Tokens, Items),
	    translate_items(Items, List, HtmlItems)
	->  true
	;   format(user_error, 'Failed to translate "~w" list~n', [List])
	).

list_command(description,     _, html('<DL>'), html('</DL>')).
list_command(dlist,	      _, html('<DL>'), html('</DL>')).
list_command(itemize,         _, html('<UL>'), html('</UL>')).
list_command(itemlist,        _, html('<UL>'), html('</UL>')).
list_command(shortlist,       _, html('<UL COMPACT>'), html('</UL>')).
list_command(enumerate,       _, html('<OL>'), html('</OL>')).
list_command(thebibliography, _, html('<DL>'), html('</DL>')).

%	items(+Tokens, -Items)
%
%	Translate the item-list of a list-environment into a nested list,
%	where each first element is the item command, and the others are
%	the tokens of the item.  

items([], []).	
items([Cmd|More], [[Cmd|ItemTokens]|Items]) :-
	functor(Cmd, \, _),
	arg(1, Cmd, TexCmd),
	tex_command_function(TexCmd, item), !,
	item_commands(More, ItemTokens, Rest),
	items(Rest, Items).
items([Token|More], List) :-
	(   no_item_token(Token)
	->  true
	;   format(user_error, 'Skipped "~w"; no item~n', [Token])
	),
	items(More, List).

%	no_item_token(+Token)
%
%	Succeeds if Tokens is a token that may appear before the first
%	item of a list, and should not be translated.

no_item_token(' ').
no_item_token('\n').
no_item_token(\par).
no_item_token(\(setlength, _)).
no_item_token(\(newcommand, _)).

item_commands(List, [], List) :-
	List = [Cmd|_],
	functor(Cmd, \, _),
	arg(1, Cmd, TexCmd),
	tex_command_function(TexCmd, item), !.
item_commands([H|T0], [H|T1], T2) :-
	item_commands(T0, T1, T2).
item_commands([], [], []).
	
%	translate_items(+ItemTokens, +List, -HTMLTokens)
%	
%	Translate the TeX tokens for a list-item.  List is passed as context,
%	to ensure proper translation.

translate_items([], _, []).
translate_items([H0|T0], List, [H1|T1]) :-
	translate(H0, List, _, H1),
	translate_items(T0, List, T1).


		 /*******************************
		 *        ACTIVE COMMANDS	*
		 *******************************/

prolog_function(\(usepackage, [_,{File},_])) :-
	(   member(Term, [tex(File), library(File)]),
	    absolute_file_name(Term,
			       [ extensions([pl, qlf]),
				 access(read),
				 file_errors(fail)
			       ],
			       PlFile)
	->  ensure_loaded(user:PlFile)
	;   true
	).
prolog_function(\(newcommand, [{Name}, [], {Expanded}])) :-
	declare_command(Name, 0, Expanded).
prolog_function(\(newcommand, [{Name}, [Args], {Expanded}])) :-
	declare_command(Name, Args, Expanded).
prolog_function(\(renewcommand, [{Name}, [], {Expanded}])) :-
	declare_command(Name, 0, Expanded).
prolog_function(\(renewcommand, [{Name}, [Args], {Expanded}])) :-
	declare_command(Name, Args, Expanded).
	

		 /*******************************
		 *	      COMMANDS		*
		 *******************************/

%
%	cmd(+Command, +Mode, -HTML)
%

cmd(onefile, preamble, []) :-
	retractall(onefile(_)),
	assert(onefile(true)).
cmd(htmlpackage({File}), preamble, []) :-
	(   absolute_file_name(tex(File),
			       [ extensions([pl, qlf]),
				 access(read),
				 file_errors(fail)
			       ],
			       PlFile)
	->  ensure_loaded(user:PlFile)
	;   format(user_error, 'Cannot find Prolog extension "~w"~n', [File])
	).
cmd(documentclass(_, _), preamble, []).
cmd(usepackage(_, {_File}, _), preamble, []) :- !.
cmd(makeindex, preamble, []) :-
	retractall(makeindex(_)),
	asserta(makeindex(true)).
cmd(vskip(_), pcecode, verb('\n')).
cmd(lineno({_Line}), pcecode, verb('\n')).

%       cmd(+Command, -HTML)

cmd(newcommand({Name}, [], {Expanded}), []) :-
	declare_command(Name, 0, Expanded).
cmd(newcommand({Name}, [Args], {Expanded}), []) :-
	declare_command(Name, Args, Expanded).
cmd(renewcommand({Name}, [], {Expanded}), []) :-
	declare_command(Name, 0, Expanded).
cmd(renewcommand({Name}, [Args], {Expanded}), []) :-
	declare_command(Name, Args, Expanded).
cmd(def({'\\booktitle'}, {Title}), HTML) :-
	cmd(title({[Title]}), HTML).
cmd(def(_, _), []).
cmd(sloppy, []).
cmd(noindent, []).
cmd(clearpage, []).
cmd(cleardoublepage, []).
cmd(nopagebreak, []).
cmd(pagebreak, []).
cmd(linebreak, [html('<BR>')]).
cmd(newpage, []).
cmd(hyphenpenalty(_), []).
cmd(newlength(_), []).
cmd(setlength(_,_), []).
cmd(settowidth(_, _), []).
cmd(setcounter({page}, _), []).
cmd(vskip(_), [html('<P>')]).
cmd(vspace(_), [html('<P>')]).
cmd(hspace(_), []).
cmd(headheight(_), []).
cmd(footheight(_), []).
cmd(vfill, [html('<P>')]).
cmd(vfil, [html('<P>')]).
cmd(hfill, []).
cmd(/, []).
cmd(and, [html('<BR>')]).
cmd(leavevmode, []).
cmd(parskip(_), []).
cmd(parindent(_), []).
cmd(raggedright, []).			% Always in HTML
cmd(tableofcontents,
    [ #tell('Contents'),
      #header(contents),
      #h(1, #label('document-contents', 'Table of Contents')),
      tableofcontents(document)
    ]).
cmd(printindex, []).

cmd(par, html('<P>')).				% \par
cmd(\([]), html('<BR>')).			% \\
cmd(\([_Skip]), html('<P>')).			% \\[skip]
cmd(newline, html('<BR>')).			% \newline

cmd(part({Title}), #h(1, +Title)).		% \part

cmd(chapter(M, {Title}), HTML) :-		% \chapter, \section, ...
	translate_section(1, M, Title, HTML).
cmd(section(M, {Title}), HTML) :-
	translate_section(2, M, Title, HTML).
cmd(subsection(M, {Title}), HTML) :-
	translate_section(3, M, Title, HTML).
cmd(subsubsection(M, {Title}), HTML) :-
	translate_section(4, M, Title, HTML).
cmd(paragraph({Title}), [#b(+Title), ' ']).
cmd(subparagraph({Title}), [#b(+Title), ' ']).

cmd(label({Label}), #label(Label, [], Tag)) :-	% \label and \xyzref
	label_tag(Tag).
cmd(ref({RefName}), #lref(RefName, ref(RefName))).
cmd(pageref({RefName}), #lref(RefName, ref(RefName))).
	
cmd(secref({Label}), HTML) :-
	translate_reference(section, sec, Label, HTML).
cmd('Secref'({Label}), HTML) :-
	translate_reference('Section', sec, Label, HTML).
cmd(chapref({Label}), HTML) :-
	translate_reference(chapter, sec, Label, HTML).
cmd('Chapref'({Label}), HTML) :-
	translate_reference('Chapter', sec, Label, HTML).
cmd(figref({Label}), HTML) :-
	translate_reference(figure, fig, Label, HTML).
cmd('Figref'({Label}), HTML) :-
	translate_reference('Figure', fig, Label, HTML).
cmd(tabref({Label}), HTML) :-
	translate_reference(table, tab, Label, HTML).
cmd('Tabref'({Label}), HTML) :-
	translate_reference('Table', tab, Label, HTML).
cmd(appref({Label}), HTML) :-
	translate_reference(appendix, sec, Label, HTML).
cmd('Appref'({Label}), HTML) :-
	translate_reference('Appendix', sec, Label, HTML).

cmd(title({Title}), []) :-			% \title
	retractall(title(_)),
	translate(Title, normal, HTML),
	assert(title(HTML)).
cmd(author({Author}), []) :-			% \author
	retractall(author(_)),
	translate(Author, normal, HTML),
	assert(author(HTML)).
cmd(bodycolor({BG}), []) :-			% \bodycolor
	retractall(bodycolor(_)),
	assert(bodycolor(BG)).
cmd(linkimage({Name}, {Icon}), []) :-		% \linkimage{Id, Path}
	make_output_directory,
	html_output_dir(Dir),
	absolute_file_name(img(Icon), Path),
	file_base_name(Path, Base),
	concat_atom([Dir, Base], /, To),
	sformat(Cmd, 'cp ~w ~w', [Path, To]),
	shell(Cmd),
	asserta(link_image(Name, Base)).
cmd(htmloutput({Dir}), []) :-
	(   done_make_output_directory
	->  format(user_error,
		   'Cannot change output directory after output has started~n',
		   []),
	    fail
	;   retract(html_output_dir(_)), !,
	    asserta(html_output_dir(Dir))
	).
cmd(htmlmainfile({File}), []) :-
	retractall(html_file_base(_)),
	assert(html_file_base(File)).
cmd(htmlfiledepth({Depth}), []) :-
	atom_codes(Depth, Chars),
	number_codes(D, Chars),
	retractall(html_split_level(_)),
	assert(html_split_level(D)).

cmd(maketitle,					% \maketitle
    #quote(#quote(#quote(#quote([ #center(#h(1, #thetitle)),
				  html('<HR>'),
				  #center(#i(#theauthor)),
				  html('<HR>')
				]))))).

cmd(newblock, []).				% BiBTeX \newblock
cmd(protect, []).				% BiBTeX produced?
cmd(citename({TeX}), HTML) :-
	translate_group([\sc|TeX], HTML).
cmd(bibitem([TeXCite], {Key}),			% \bibitem
    [ html('<P>'), html('<DT>'), #label(Key, #strong(Cite)), html('<DD>') ]) :-
	translate(TeXCite, normal, Cite),
	assert(cite(Key, Cite)).
cmd(bibitem([], {Key}), 
    [ html('<P>'), html('<DT>'), #label(Key, #strong(Cite)), html('<DD>') ]) :-
	flag(cite, N, N+1),
	TeXCite is N + 1,
	expand_macros(#embrace("[]", TeXCite), Cite),
	assert(cite(Key, Cite)).
cmd(cite({Key}),	#cite(Key)).		% \cite
cmd(yearcite({Key}),	#yearcite(Key)).	% \yearcite
cmd(opencite({Key}),	#opencite(Key)).	% \opencite
cmd(bibliography({_}), HTML) :-			% \bibliography
	tex_file_base(File),
	(   absolute_file_name(tex(File), [ extensions([bbl]),
					    access(read),
					    file_errors(fail)
					  ],
			       BiBFile)
	->  tex_tokens(BiBFile, TeXTokens),
	    translate(TeXTokens, file, HTML)
	;   format(user_error, 'No bibliography file~n', []),
	    HTML = []
	).
cmd(bibliographystyle(_), []).			% \bibliographystyle
cmd(',', []).					% \,
cmd(-, []).					% \- (stop hyphenation)

cmd(emph({Tex}),   #em(+Tex)).			% \emph{text}
cmd(texttt({Tex}), #tt(+Tex)).			% \texttt{text}
cmd(textbf({Tex}), #b(+Tex)).			% \textbf{text}
cmd(textit({Tex}), #i(+Tex)).			% \textit{text}
cmd(textsf({Tex}), #b(+Tex)).			% \textsf{text}
cmd(textsc({Tex}), #sc(+Tex)).			% \textsc{text}

cmd(year,	Year) :-			% \year
	get_time(Time),
	convert_time(Time, Year, _, _, _, _, _, _).
cmd('LaTeX',	'LaTeX').			% \LaTeX
cmd('TeX',	'TeX').				% \TeX

cmd(index({Term}), #label(RefName, [])) :-	% \index
	translate_index(Term, RefName).
cmd(idx({Term}), #label(RefName, Term)) :-	% \idx
	translate_index(Term, RefName).

cmd(footnote({TeX}), HTML) :-			% \footnote
	translate_footnote(TeX, HTML).
cmd(footnotetext([Num], {Tokens}), [html('<P>'), #embrace(Num), Text]) :-
	translate(Tokens, normal, Text).

cmd(pagenumbering(_), []).			% pagestyle stuff
cmd(pagestyle(_),     []).
cmd(thispagestyle(_), []).
cmd(fancyplain(_),    []).
cmd(lhead(_,_),	      []).
cmd(chead(_,_),	      []).
cmd(rhead(_,_),	      []).
cmd(lfoot(_,_),	      []).
cmd(cfoot(_,_),	      []).
cmd(rfoot(_,_),	      []).
cmd(rightmark,	      []).
cmd(leftmark,	      []).
cmd(footrulewidth(_), []).

cmd(centerline({Tex}), #center(+Tex)).
cmd(rightline({Tex}), #right(+Tex)).

cmd(email([], {Address}), #url(URL, Address)) :-
	sformat(URL, 'mailto:~w', [Address]).
cmd(email([Text], {Address}), #url(URL, +Text)) :-
	sformat(URL, 'mailto:~w', [Address]).
cmd(url([], {Address}), #url(Address, Address)).
cmd(url([Text], {Address}), #url(Address, +Text)).
cmd(file({File}), #tt(File)).
cmd(strong(		{A1}), #strong(+A1)).
cmd(tick({Tokens}),
    [ html('<LI>'), html('<I>'), Tag, html('</I>'), html('<BR>') ]) :-
	translate_group(Tokens, Tag).
cmd(item([]), html('<LI>')).
cmd(item([Tag]),
    [ html('<LI>'), html('<I>'), +Tag, html('</I>'), html('<BR>') ]).
cmd(mbox({Boxed}), HTML) :-
	translate_group(Boxed, HTML).
cmd(makebox(_, _, {Boxed}), HTML) :-
	translate_group(Boxed, HTML).
cmd(parbox(_, _, {Boxed}), HTML) :-
	translate_group(Boxed, HTML).
cmd(string({Text}), nospace(Text)).
cmd(ldots, '...').
cmd(cline(_), []).
cmd('%', nospace('%')).
cmd('#', nospace('#')).
cmd('$', nospace('$')).
cmd('&', nospace('&')).
cmd('{', nospace('{')).
cmd('}', nospace('}')).
cmd('[', nospace('[')).
cmd(']', nospace(']')).
cmd('"'({'\\i'}), html('&iuml;')).	% \"\i
cmd('"'({C}), html(Cmd)) :-		% \"[ouey...]
	concat_atom([&, C, 'uml;'], Cmd).
cmd(''''({C}), html(Cmd)) :-		% \'[ouey...]
	concat_atom([&, C, 'acute;'], Cmd).
cmd(' ', nospace(' ')).			% :-)
cmd(copyright, html('&copy;')).		% \copyright
cmd(tm, html('&reg;')).			% \tm
cmd(alpha, html('&alpha;')).
cmd(beta, html('&beta;')).
					% old stuff (< HTML 3)
cmd(alpha, #var(a)).			% \alpha
cmd(beta, #var(b)).			% \beta
cmd(tm, #embrace(tm)).			% \tm

cmd(include({File}), HTML) :-
	absolute_file_name(tex(File), [ extensions([tex]),
					access(read),
					file_errors(true)
				      ],
			   TeXFile),
	tex_tokens(TeXFile, TeXTokens),
	translate(TeXTokens, file, HTML).
cmd(input({File}), []) :-
	file_name_extension(_, sty, File), !.
cmd(input({File}), HTML) :-
	absolute_file_name(tex(File), [ extensions([tex, '']),
					access(read),
					file_errors(true)
				      ],
			   TeXFile),
	tex_tokens(TeXFile, TeXTokens),
	translate(TeXTokens, file, HTML).
cmd(appendix, []) :-
	appendix.
%cmd(caption({Caption}),
%    #center([#b([#nameof(Type), ' ', Number, ':', ' ']), +Caption])) :-
%	current_float(Type, Number).
cmd(caption({Caption}),
    [ html('<TABLE ALIGN=center WIDTH="75%"><TR><TD>'),
      #b([#nameof(Type), ' ', Number, ':', ' ']),
      +Caption,
      html('</TABLE>')
    ]) :-
	current_float(Type, Number).

cmd(psdirectories({Spec}), []) :-
	split(Spec, ",", Dirs),
	retractall(user:file_search_path(psfig, _)),
	forall(member(D, Dirs),
	       assert(user:file_search_path(psfig, tex(D)))).
cmd(psfig({Spec}), html(Img)) :-
	psfig_options(Spec, Options),
	member(figure(File), Options),
	file_name_extension(Base, Ext, File),
	ps_extension(Ext),
	file_base_name(Base, GifBase),
	file_name_extension(GifBase, gif, GifFile),
	sformat(Img, '<IMG SRC="~w">', GifFile),
	make_output_directory,
	html_output_dir(Dir),
	concat_atom([Dir, '/', GifFile], OutFile),
	(   keep_figures(true),
	    exists_file(OutFile)
	->  true
	;   (   is_absolute_file_name(Base)
	    ->	FileSpec = Base
	    ;	FileSpec = tex(Base)
	    ),
	    ps2gif(FileSpec, OutFile)
	).
cmd(includegraphics(_Options, {File}), html(Img)) :-
	ps_extension(Ext),
	absolute_file_name(includegraphics(File),
			   [ extensions([Ext]),
			     access(read)
			   ], PsFile),
	file_name_extension(Base, Ext, PsFile),
	file_base_name(Base, GifBase),
	file_name_extension(GifBase, gif, GifFile),
	sformat(Img, '<IMG SRC="~w">', GifFile),
	make_output_directory,
	html_output_dir(Dir),
	concat_atom([Dir, '/', GifFile], OutFile),
	(   keep_figures(true),
	    exists_file(OutFile)
	->  true
	;   ps2gif(PsFile, OutFile)
	).
cmd(postscript({_Width}, {File}, Title),
    [ LabelHTML,
      #center(html(Img)),
      html('<P>'),
      Caption
      ]) :-
	file_name_extension(File, gif, GifFile),
	concat('fig:', File, Label),
	step_counter(figure, Fig),
	do_float(figure(Fig),
		 (   translate_command(caption(Title), float, _, Caption),
		     translate_command(label({Label}), float, _, LabelHTML)
		 )),
	sformat(Img, '<IMG SRC="~w">', GifFile),
	make_output_directory,
	current_setting(html_output_dir(Dir)),
	concat_atom([Dir, '/', GifFile], OutFile),
	(   current_setting(keep_figures),
	    exists_file(OutFile)
	->  true
	;   ps2gif(psfig(File), OutFile, [margin(5)])
	).
cmd(postscriptfig(_Options, {File}, Title),
    [ LabelHTML,
      #center(html(Img)),
      html('<P>'),
      Caption
    ]) :-
	file_name_extension(File, gif, GifFile),
	concat('fig:', File, Label),
	step_counter(figure, Fig),
	do_float(figure(Fig),
		 (   translate_command(caption(Title), float, _, Caption),
		     translate_command(label({Label}), float, _, LabelHTML)
		 )),
	sformat(Img, '<IMG SRC="~w">', GifFile),
	make_output_directory,
	current_setting(html_output_dir(Dir)),
	concat_atom([Dir, '/', GifFile], OutFile),
	(   current_setting(keep_figures),
	    exists_file(OutFile)
	->  true
	;   ps2gif(psfig(File), OutFile, [margin(5)])
	).

ps_extension(eps).
ps_extension(ps).

%
%	HTML documentation
%

cmd('HTML'({Tag}),	#code(Tag)).
cmd(latexcmd({Cmd}),	#code(BslCmd)) :-
	concat(\, Cmd, BslCmd).
cmd(latexenv({Env}),	#code(Env)).
cmd(mode({Mode}),	#code(Mode)).


%
%	cmd(+Command, +Mode, -HTML
%

cmd(item([Tag]), description,			% \item in description
    [ html('<DT>'), #b(+Tag), html('<DD>') ]).
cmd(item([Tokens]), itemlist,			% \item in itemlist
    [ html('<LI>'), html('<I>'), Tag, html('</I>'), html('<BR>') ]) :-
	translate_group(Tokens, Tag).

cmd(times, math, [' ', html('&times;'), ' ']).
cmd(wedge, math, ['/\\']).
cmd(rightarrow, ['->']).
cmd('Rightarrow', ['=>']).
cmd('Leftrightarrow', ['<=>']).
cmd(pi, math, 'pi').
%cmd(circ, math, html('&omicron;')).		% not in Netscape 4.51
cmd(circ, math, o).
cmd(rhd, math, html('&gt;')).
cmd(leq, math, '=<').
cmd(equiv, math, '==').
cmd(longrightarrow, math, '-->').
cmd(geq, math, '>=').
cmd(ge, math, '>=').
cmd(pm, math, html('&#177;')).
cmd(langle, math, '<').
cmd(rangle, math, '>').
cmd(sin({Arg}), math, ['sin(', math+Arg, ')']).
cmd(cos({Arg}), math, ['cos(', math+Arg, ')']).
cmd(tan({Arg}), math, ['tan(', math+Arg, ')']).
cmd(arcsin({Arg}), math, ['arcsin(', math+Arg, ')']).
cmd(arccos({Arg}), math, ['arccos(', math+Arg, ')']).
cmd(arctan({Arg}), math, ['arctan(', math+Arg, ')']).
cmd(ln({Arg}), math, ['ln(', math+Arg, ')']).
cmd(lg({Arg}), math, ['log10(', math+Arg, ')']).
cmd(sqrt({Arg}), math, ['sqrt(', math+Arg, ')']).
cmd(not({=}), math, ['<>']).
cmd(neq, math, ['<>']).
cmd(exists, math, 'E').
cmd(emptyset, match, html('&Oslash;')).
cmd(subset, match, 'subset').
cmd(frac({A1}, {A2}), math, [math+A1, '/', math+A2]).
cmd(mod({A1}, {A2}), math, [math+A1, ' ', 'mod', ' ', math+A2]).
cmd(rem({A1}, {A2}), math, [math+A1, ' ', 'rem', ' ', math+A2]).
cmd(div, math, div).
cmd(pow({A1}, {A2}), math, [math+A1, '**', math+A2]).
cmd(tt, math, []).			% just ignore?

%
% cmd(+Command, +Mode0, -Mode, -HTML
%

cmd(Cmd, Mode0, Mode, HTML) :-
	user_cmd(Cmd, Mode0, Mode, HTML), !.
cmd(obeylines, group(Atts), group([obeylines|Atts]), []).
cmd(Font, group(Old), group([font(Font)|Old1]), HTML) :-
	html_font(Font, Open, _), !,
	(   select(font(OldFont), Old, Old1),
	    html_font(OldFont, _, Close)
	->  HTML = [Close,Open]
	;   Old1 = Old,
	    HTML = Open
	).

html_font(em, 		html('<EM>'), 		html('</EM>')).
html_font(bf, 		html('<B>'),  		html('</B>')).
html_font(it, 		html('<I>'),  		html('</I>')).
html_font(cal, 		html('<I>'),  		html('</I>')).
html_font(tt, 		html('<TT>'), 		html('</TT>')).
html_font(sf, 		html('<B>'),  		html('</B>')).
html_font(sc, 		html('<font size=-1>'), html('</font>')).
html_font(rm, 		[],           		[]).
html_font(sl, 		html('<B>'),  		html('</B>')).
html_font(scriptsize,   [], 			[]).
html_font(footnotesize, html('<font size=-1>'), html('</font>')).
html_font(small,        html('<font size=-1>'), html('</font>')).
html_font(normalsize,   [],			[]).
html_font(large,        html('<font size=+1>'), html('</font>')).
html_font('Large',      html('<font size=+2>'), html('</font>')).
html_font('Huge',       html('<font size=+3>'), html('</font>')).


		 /*******************************
		 *	    \REF, ETC.		*
		 *******************************/

%	translate_reference(+Name, +Tag, +Label, -HTML)
%
%	Used for the translation of \secref, \figref, etc.

translate_reference(Name, sec, Label,
	  #lref(fileof(RefName), [Name, ' ', ref(RefName)])) :- !,
	sformat(RefStr, 'sec:~w', [Label]),
	string_to_atom(RefStr, RefName).
translate_reference(Name, Tag, Label,
	  #lref(RefName, [Name, ' ', ref(RefName)])) :-
	sformat(RefStr, '~w:~w', [Tag, Label]),
	string_to_atom(RefStr, RefName).


		 /*******************************
		 *	      TTY STUFF		*
		 *******************************/

%	clean_tt(+Raw, -Clean)
%
%	Cleans the TeX escapes to write down weird predicate names from
%	the input.

clean_tt([Atom], Atom) :-
	atomic(Atom), !.
clean_tt(Raw, Clean) :-
	atom_codes(Raw, S0),
	(   append("{", S1, S0),
	    append(S2, "}", S1)
	->  true
	;   S2 = S0
	),
	(   append("\\tt", S3, S2)
	->  true
	;   S3 = S2
	),
        clean_specials(S3, S4),
	replace_all(S4, "\\bsl{}", "\\", S5),
	delete_all(S5, "\\string", S6),
	delete_all(S6, " ", S7),
	replace_all(S7, "~", " ", S8),
	atom_codes(Clean, S8).

clean_specials([], []).
clean_specials([0'\\, Special|T0], [Special|T]) :-
	memberchk(Special, "#$&%{}"), !,
	clean_specials(T0, T).
clean_specials([H|T0], [H|T]) :-
	clean_specials(T0, T).

delete_all(S0, D, S) :-
	(   append(D, Post, P2)
	->  (   append(P1, P2, S0)
	    ->	append(P1, Post, S1),
		delete_all(S1, D, S)
	    ;	S = S0
	    )
	).

replace_all(S0, F, T, S) :-
	(   append(F, Post, P2)
	->  (   append(P1, P2, S0)
	    ->	append(P1, T, S1),
		append(S1, Post, S2),
		replace_all(S2, F, T, S)
	    ;	S = S0
	    )
	).


		 /*******************************
		 *	     CASE (\SC)		*
		 *******************************/

upcase_html([], []).
upcase_html([H0|T0], [H|T]) :-
	atomic(H0), !,
	upcase_atom(H0, H),
	upcase_html(T0, T).
upcase_html([H|T0], [H|T]) :-
	upcase_html(T0, T).

capitalise_atom(In, Out) :-
	atom_codes(In, S0),
	capitalise(S0, S1, up),
	atom_codes(Out, S1).

capitalise([], [], _).
capitalise([H0|T0], [H|T], up) :-
	is_lower(H0), !,
	to_upper(H0, H),
	capitalise(T0, T, down).
capitalise([H|T0], [H|T], up) :-
	capitalise(T0, T, up).
capitalise([H|T0], [H|T], down) :-
	is_alpha(H), !,
	capitalise(T0, T, down).
capitalise([H|T0], [H|T], down) :-
	capitalise(T0, T, up).
	

		 /*******************************
		 *	    FOOTNOTES		*
		 *******************************/

:- dynamic
	footnote/2.			% Id, Tokens

reset_footnotes :-
	retractall(footnote(_, _)).


translate_footnote(Tokens,
		   #lforw(Tag, [' ', #embrace(Value)])) :-
	step_counter(footnote_counter, Value),
	sformat(Tag, 'note-~d', [Value]),
	assert(footnote(Tag, Tokens)).

footnote_html([]) :-
	\+ footnote(_, _), !.
footnote_html(HTML) :-
	HTML0 = [ #tell('Notes'),
		  #header(notes),
		  #h(1, #label('document-notes', 'Footnotes')),
		  html('<DL>'),
		  FootNodes,
		  html('</DL>')
		],
	findall(FN, footnote_html1(FN), FootNodes),
	expand_macros(HTML0, HTML).

footnote_html1([ html('<DT>'),
		 #lback(Tag, Tag),
		 html('<DD>'),
	         +TeX
	       ]) :-
	footnote(Tag, TeX).
	

		 /*******************************
		 *	       SECTION		*
		 *******************************/

:- dynamic
	section_counter_array/1,	% counters(L0, L1, ...)
	section/3,			% Level, Tag, Title
	appendix_section/1.		% First number of appendix

reset_sections :-
	retractall(section(_,_,_)),
	retractall(section_counter_array(_)),
	retractall(appendix_section(_)).

appendix :-
	section_counter_array(Counters),
	arg(1, Counters, Chapter),
	asserta(appendix_section(Chapter)).

%	trans_section(+SectionCmd, +TexTitle, +RestDocument, -HTML
%
%	Allow look-ahead for \label{}, so the label can be used to
%	generate the section filename.

trans_section(Section, Title, Rest, HTML) :-
	section_level(Section, Level),
	ignore(find_label(Rest, Label)),
	translate_section(Level, -, Title, HTML, Label).

section_level(chapter,	     1).
section_level(section,	     2).
section_level(subsection,    3).
section_level(subsubsection, 4).

find_label([' '|T], Label) :- !,
	find_label(T, Label).
find_label(['\n'|T], Label) :- !,
	find_label(T, Label).
find_label([\par|T], Label) :- !,
	find_label(T, Label).
find_label([\(label, [{RawLabel}])|_], Label) :-
	(   sub_atom(RawLabel, _, _, A, :)
	->  sub_atom(RawLabel, _, A, 0, Label)
	;   Label = RawLabel
	).
	
%	translate_section(+Level, +Modify, +TitleTokens, -HTML[, +File])

translate_section(Level, Mod, TexTitle, HTML) :-
	translate_section(Level, Mod, TexTitle, HTML, _).

translate_section(Level, -, TeXTitle,
	[ Footer,
	  Tell,
	  Header,
	  #h(Level, #label(RefName, [Tag, ' ', Title]))
	], NodeFile) :- !,
	translate(TeXTitle, normal, Title),
	section_tag(OldTag),
	increment_section_counter(Level, Tag, FirstSubSection),
	(   html_split_level(Split),
	    Level =< Split
	->  Tell = #tell(NodeFile),
	    Header = #header(Tag),
	    (	var(NodeFile)
	    ->  section_file(Tag, Title, NodeFile)
	    ;	true
	    ),
	    (   FirstSubSection
	    ->  Footer = #footer(OldTag)
	    ;   Footer = []
	    )
	;   Tell = [],
	    Footer = [],
	    Header = []
	),
	sformat(RefName, 'sec:~w', [Tag]),
	assert(section(Level, Tag, Title)).
translate_section(Level, *, Title, #h(Level, +Title), _).

h(1, '<H1>', '</H1>').
h(2, '<H2>', '</H2>').
h(3, '<H3>', '</H3>').
h(4, '<H4>', '</H4>').
h(5, '<H5>', '</H5>').
h(6, '<H6>', '</H6>').

section_file(Tag, _, Node) :-
	atom_concat('sec-', Tag, Node).

increment_section_counter(Level, Tag, FirstSubSection) :-
	(   retract(section_counter_array(Old))
	->  true
	;   Old = counters(0,0,0,0,0,0)
	),
	functor(New, counters, 6),
	update_section_counter(1, 6, Level, Old, New, FirstSubSection),
	asserta(section_counter_array(New)),
	section_tag(Tag).

update_section_counter(I, M, L, Old, New, FirstSubSection) :-
	(   I =< M
	->  (   I == L
	    ->  arg(I, Old, N),
		NN is N + 1,
		arg(I, New, NN),
		(   N == 0
		->  FirstSubSection = true
		;   FirstSubSection = fail
		)
	    ;   I < L
	    ->  arg(I, Old, N),
		arg(I, New, N)
	    ;   arg(I, New, 0)
	    ),
	    NI is I + 1,
	    update_section_counter(NI, M, L, Old, New, FirstSubSection)
	;   true
	).

section_tag(Tag) :-
	section_counter_array(Term), !,
	findall(A, (arg(_, Term, A), A > 0), L),
	(   appendix_section(AS),
	    arg(1, Term, S),
	    S > AS
	->  App is (S - AS - 1) + 0'A,
	    char_code(AN, App),
	    L = [_|T],
	    concat_atom([AN|T], '.', Tag)
        ;   concat_atom(L, '.', Tag)
	).
section_tag('').

parent_tag(Section, Parent) :-
	atom_codes(Section, Chars),
	phrase(parent_section(Parent), Chars), !.

parent_section(Parent) -->
	string(ParentString),
	".",
	integer(_),
	{atom_codes(Parent, ParentString)}.

:- dynamic
	section_level/1.

tableofcontents([]) :-
	\+ section(_,_,_), !.
tableofcontents(Sections) :-
	tableofcontents('', Sections).

tableofcontents(TagPrefix, [Sections, CloseUL]) :-
	retractall(section_level(_)),
	(   section(Level, Tag, _Title),
	    sub_atom(Tag, 0, _, _, TagPrefix)
	->  L0 is Level - 1
	;   L0 = 0
	),
	asserta(section_level(L0)),
	findall(S, section_html(TagPrefix, S), Sections),
	fix_level(L0, CloseUL).

section_html(TagPrefix,
	     [ FixLevel,
	       html('<LI>'),
	       #lref(Ref, [Open, Tag, ' ', Title, Close])
	     ]) :-
	section(Level, Tag, Title),
	html_split_level(Split),
	(   Level =< Split
	->  Ref = fileof(RefName)
	;   Ref = RefName
	),
	concat(TagPrefix, _, Tag),
	fix_level(Level, FixLevel),
	sformat(RefName, 'sec:~w', [Tag]),
	contents_line_style(Level, Open, Close).

fix_level(To, []) :-
	section_level(To), !.
fix_level(To, HTML) :-
	(   retract(section_level(From))
	->  fix_indent(To, From, HTML),
	    asserta(section_level(To))
	).

fix_indent(L, L, []) :- !.
fix_indent(To, From, [html('<UL>')|T]) :-
	To > From, !,
	NFrom is From + 1,
	fix_indent(To, NFrom, T).
fix_indent(To, From, [html('</UL>')|T]) :-
	To < From, !,
	NFrom is From - 1,
	fix_indent(To, NFrom, T).

	
contents_line_style(1, html('<STRONG>'), html('</STRONG>')) :- !.
contents_line_style(2, html('<B>'),      html('</B>')) :- !.
contents_line_style(_, [],      	 []).

		 /*******************************
		 *	       INDEX		*
		 *******************************/

:- dynamic
	index/3.			% SortKey, Word, Tag

reset_index :-
	retractall(index(_,_, _)).

translate_index(Term, RefName) :-
	step_counter(index, N),
	clean_index(Term, Clean),
	sformat(RefName, 'idx:~w:~w', [Clean, N]),
	section_tag(Tag),
	add_to_index(Term, Tag:RefName).

clean_index(Raw, Cleaned) :-
	atom_codes(Raw, RawChars),
	clean_index_2(RawChars, Chars),
	atom_codes(Cleaned, Chars).

clean_index_2([], []).
clean_index_2([H|T0], [H|T]) :-
	valid_index_char(H), !,
	clean_index_2(T0, T).
clean_index_2([_|T0], T) :-
	clean_index_2(T0, T).

valid_index_char(C) :-
	between(0'a, 0'z, C).
valid_index_char(C) :-
	between(0'A, 0'Z, C).
valid_index_char(C) :-
	between(0'0, 0'9, C).

add_to_index(Term) :-
	section_tag(Tag),
	add_to_index(Term, Tag).

%	add_to_index(+Term, +Tag)
%	
%	Add Term to the index using the href Tag. If Tag is of the
%	format +Tag, it is the primary index for the term, normally
%	a pointer to the definition of Term.

add_to_index(Term, Tag) :-
	atom_codes(Term, Chars),
	atom_codes(Atom, Chars),	% So, sure we have an atom now
	sort_chars(Chars, Sort),
	atom_codes(SortKey, Sort),
	assert(index(SortKey, Atom, Tag)).

%sort_chars(Chars, Sort) :-
%	append("menu:", X, Chars), !,
%	sort_chars(X, Sort).
sort_chars(Chars, Sort) :-
	member(C, Chars),
	is_alpha(C), !,
	get_lower_letters(Chars, Sort).
sort_chars(Chars, Chars).

get_lower_letters([], []).
get_lower_letters([H0|T0], [H|T]) :-
	is_alpha(H0), !,
	to_lower(H0, H),
	get_lower_letters(T0, T).
get_lower_letters([_|T0], T) :-
	get_lower_letters(T0, T).


make_index([]) :-
	makeindex(false), !.
make_index([]) :-
	\+ index(_, _, _), !.
make_index(HTML) :-
	HTML0 = [ #tell('DocIndex'),
		  #header(index),
		  #h(1, #label('document-index', 'Index')),
		  html('<DL>'),
		  Index,
		  html('</DL>')
		],
	setof(I, T^Tag^index(I, T, Tag), I0),
	index_html(I0, 0, Index),
	expand_macros(HTML0, HTML).

index_html([], _, []).
index_html([SortKey|T0], CL0, [Sep, TermHTML|TH]) :-
	add_separator(SortKey, CL0, CL, Sep),
	setof(Term, Tag^index(SortKey, Term, Tag), Terms),
	index_terms(Terms, TermHTML),
	index_html(T0, CL, TH).
	
index_terms([], []).
index_terms([Term0|T0], [ html('<DT>'),
			 HtmlTerm,
			 html('<DD>'),
			 Where
		       | TH
		       ]) :-
	findall(Tag, index(_, Term0, Tag), Tags),
	tex_atom_to_tokens(Term0, Tokens),
	translate(Tokens, index, _, Term),
	(   member(+PrimeTag, Tags)
	->  HtmlTerm = #lref(PrimeTag, Term)
	;   HtmlTerm = Term
	),
	maplist(index_href, Tags, Where),
	index_terms(T0, TH).

index_href(+(_), []) :- !.
index_href(Tag:Label, [' ', #lref(Label, Tag)]) :- !.
index_href(Tag, [' ', #lref(RefName, Tag)]) :-
	sformat(RefName, 'sec:~w', Tag).
	
add_separator(Term, CL, CL, []) :-
	atom_codes(Term, [CL|_]), !.
add_separator(Term, _, CL, [ html('<DT>'), 
			     #strong(Char),
			     html('<DD>')
			   ]) :-
	atom_codes(Term, [CL|_]),
	to_upper(CL, UC),
	char_code(Char, UC).


		 /*******************************
		 *        STANDARD LINKS	*
		 *******************************/

node_header([#head(#title(#thetitle)),
	     #beginbody
	    ]) :-
	onefile(true), !.
node_header([#head([#title(#thetitle),
		    link(home),
		    link(contents),
		    link(index),
		    link(summary),
		    link(previous),
		    link(next)]),
	     #beginbody,
	     html('<HR>'),
	     #center( [ body_link(home),
			body_link(contents),
			body_link(index),
			body_link(summary),
			body_link(previous),
			body_link(next)
		      ]),
	     html('<HR>')
	    ]).
	
node_header(_, []) :-
	onefile(true), !.
node_header(SectionTag,
	    [#head([#title([#thetitle, nospace(:), ' ',
			    'Section', SectionTag]),
		    link(home),
		    link(contents),
		    link(index),
		    link(summary),
		    link(up(UpRef)),
		    link(previous),
		    link(next)]),
	     #beginbody,
	     html('<HR>'),
	     #center( [ body_link(home),
			body_link(contents),
			body_link(index),
			body_link(summary),
			body_link(up(UpRef)),
			body_link(previous),
			body_link(next)
		      ]),
	     html('<HR>')
	    ]) :-
	parent_tag(SectionTag, UpTag), !,
	sformat(UpRef, 'sec:~w', [UpTag]).
node_header(_, HTML) :-
	node_header(HTML).

	
node_footer(Tag, tableofcontents(section(Tag))).


subsection_index(Tag,
	    [ html('<HR>'),
	      #center([html('<H2>'), 'Section Index', html('</H2>')]),
	      html('<HR>'),
	      SubIndex
	    ]) :-
	onefile(false),
	Tag \== '',
	concat(Tag, '.', Filter),
	tableofcontents(Filter, SubIndex),
	submember(#lref(_,_), SubIndex), !. % it is not empty
subsection_index(_, []).

submember(X, [X|_]).
submember(X, [L|_]) :-
	is_list(L),
	submember(X, L).
submember(X, [_|T]) :-
	submember(X, T).


		 /*******************************
		 *	     COUNTERS		*
		 *******************************/

:- dynamic
	counter/2.				% Name, Value

reset_counters :-
	retractall(counter(_,_)).

set_counter(Name, Val) :-
	retractall(counter(Name, _)),
	asserta(counter(Name, Val)).


step_counter(Name, NewVal) :-
	(   retract(counter(Name, OldVal))
	->  NewVal is OldVal + 1
	;   NewVal is 1
	),
	asserta(counter(Name, NewVal)).

		 /*******************************
		 *	       RULES		*
		 *******************************/

cmd(rule({'\\linewidth'}, {_H}), [html('<hr>')]) :- !.
cmd(rule({_W}, {_H}), []) :-
	format('_W = ~w, H = ~w~n', [_W, _H]).

		 /*******************************
		 *	      TABLES		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Specifying the number of columns makes Netscape make the columns equally
width.  Thats not what we want.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

translate_table(Format, Body, HTML) :-
	atom_codes(Format, Fmt),
	table_frame(Fmt, Body, FrameAttributes, Fmt2, Body2),
	expand_table_commands(Body2, Body3),
	(   table_columns(Fmt2, Ncols, ColAtts)
	->  true
	;   format(user_error, 'Failed to parse tabular spec "~s"~n', [Fmt2]),
	    ColAtts = [],
	    Ncols = 0
	),
	(   table_body(Body3, ColAtts, BodyHTML)
	->  true
	;   format(user_error, 'Failed to translate table body~n', []),
	    trace, fail
	),
	HTML = [ html(Head),
%		 ColHTML,
		 BodyHTML,
		 html('</TABLE>')
	       ],
	(   FrameAttributes == void
	->  Border = 0
	;   Border = 2
	),
%	sformat(Head, '<TABLE BORDER=~d COLS=~d FRAME=~w RULES=groups>',
%		[Border, Ncols, FrameAttributes]).
	sformat(Head, '<TABLE BORDER=~d FRAME=~w RULES=groups>',
		[Border, FrameAttributes]).

%	expand_table_commands(+BodyIn, -BodyOut)
%
%	This shouldn't be here, but it translates commands that expand
%	into the table-special commands & or \\.

expand_table_commands([], []).
expand_table_commands([\isa|T0], [&, '::=', &|T]) :- !,
	expand_table_commands(T0, T).
expand_table_commands([\ora|T0], [&,   '|', &|T]) :- !,
	expand_table_commands(T0, T).
expand_table_commands([H|T0], [H|T]) :-
	expand_table_commands(T0, T).

%	table_frame(+Format, +Body, -FrameAttributes, -Format2, -Body2)
%
%	Extracts the frame attributes, controlling the border of the
%	table from the format and opening/closing \hline.

table_frame(Fmt, Body, TableAttributes, Fmt2, Body2) :-
	v_table_frame(Fmt, VFr, Fmt2),
	clean_body(Body, Body1),
	h_table_frame(Body1, HFr, Body2),
	table_frame(VFr, HFr, TableAttributes).
	
v_table_frame([0'||Fmt0], VFr, Fmt) :- !,
	(   append(Fmt, [0'|], Fmt0)
	->  VFr = vsides		% |cols|
	;   Fmt = Fmt0,			% |cols
	    VFr = lhs
	).
v_table_frame(Fmt0, VFr, Fmt) :-
	(   append(Fmt, [0'|], Fmt0)
	->  VFr = rhs			% cols|
	;   Fmt = Fmt0,			% cols
	    VFr = void
	).

h_table_frame([\hline|Body0], VFr, Body) :- !,
	(   (   append(Body, [\hline], Body0)
	    ;	append(Body, [\hline,\(\, _)], Body0)
	    )
	->  VFr  = hsides		% \hline body \hline [\\]
	;   Body = Body0,		% \hline body
	    VFr  = above
	).
h_table_frame(Body0, HFr, Body) :-
	(   (   append(Body, [\hline], Body0)
	    ;	append(Body, [\hline,\(\, _)], Body0)
	    )
	->  HFr  = below		% body \hline [\\]
	;   Body = Body0,		% body
	    HFr  = void
	).

table_frame(X, void, X) :- !.
table_frame(void, X, X) :- !.
table_frame(vsides, hsides, box) :- !.
table_frame(X, Y, border) :-
	format(user_error,
	       'Cannot combine ~w and ~w for table border~n', [X, Y]).
	       

table_columns(Fmt, Ncols, Cols) :-
	table_columns(Fmt, 0, Ncols, Cols).

table_columns([],      Ncols, Ncols, []).
table_columns([0'l|T], NC0,   NC,    [[]|TH]) :-
	NC1 is NC0 + 1,
	table_columns(T, NC1, NC, TH).
table_columns([0'c|T], NC0,   NC,    [['ALIGN'=center]|TH]) :-
	NC1 is NC0 + 1,
	table_columns(T, NC1, NC, TH).
table_columns([0'r|T], NC0,   NC,    [['ALIGN'=right]|TH]) :-
	NC1 is NC0 + 1,
	table_columns(T, NC1, NC, TH).
table_columns([0'p|T0], NC0,   NC,   [Col|TH]) :-
	phrase(parbox_width(_W), T0, T),
	Col = [],
/*	(   integer(W)
	->  Col = ['WIDTH'=W]
	;   append(Done, T, T0),
	    format(user_error,
		   'Could not determing column width from "~s"~n', [Done]),
	    Col = []
	),
*/
	NC1 is NC0 + 1,
	table_columns(T, NC1, NC, TH).
table_columns([0'D|T0], NC0,   NC,   [['ALIGN'=char, 'CHAR'=Chr]|TH]) :-
	phrase(align_char(Chr), T0, T),
	NC1 is NC0 + 1,
	table_columns(T, NC1, NC, TH).
table_columns([0'||T], NC0,  NC,     TH) :-
	table_columns(T, NC0, NC, TH).

parbox_width(W) -->
	"{",
	number(N),
	dimension_unit(U),
	"}", !,
	{ relative_width(U, N, W) }.
parbox_width(-) -->
	"{",
	string_without("{}", _),
	"}", !.

align_char(Chr) -->		% D{inputsep}{outputsep}{decimal places}
	"{",
	string_without("{}", W),
	"}{",
	string_without("{}", _),
	"}{",
	number(_),
	"}",
	{ atom_codes(Chr, W)
	}.

string_without(L, [C|T]) -->
	[C],
	{\+ member(C, L)}, !,
	string_without(L, T).
string_without(_, []) --> [].

number(N) -->
	optional_sign(S),
	digit(C0),
	float_digits(C),
	{ append(S, [C0|C], Chars),
	  number_codes(N, Chars)
	}.

integer(N) -->
	optional_sign(S),
	digit(C0),
	digits(C),
	{ append(S, [C0|C], Chars),
	  number_codes(N, Chars)
	}.

optional_sign("-") --> "-", !.
optional_sign("+") --> "+", !.
optional_sign("")  --> "".

digit(C) -->
	[C],
	{between(0'0, 0'9, C)}.

digits([C0|C]) -->
	digit(C0), !,
	digits(C).
digits([]) --> [].

float_digits([C0|C]) -->
	digit(C0), !,
	float_digits(C).
float_digits([0'.|C]) -->
	".", !,
	float_digits(C).
float_digits([]) --> [].


dimension_unit(in) --> "in".
dimension_unit(pt) --> "pt".
dimension_unit(cm) --> "cm".
dimension_unit(mm) --> "mm".

relative_width(in, N, W) :- W is integer(100 * N / 6).
relative_width(pt, N, W) :- W is integer(100 * (N/72) / 6).
relative_width(cm, N, W) :- W is integer(100 * (N/2.54) / 6).
relative_width(mm, N, W) :- W is integer(100 * (N/25.4) / 6).

%	The VALIGN=top should be at the table-level, but Netscape doesn't
%	appear to recognise this.  It is just LaTeX's default, while HTML's
%	default is middle.


table_body([], _, []).
table_body([\(\, _)|T], _, []) :-
	all_white_space(T).
table_body([' '|T0], ColAtts, T) :-
	table_body(T0, ColAtts, T).
table_body(['\n'|T0], ColAtts, T) :- !,
	table_body(T0, ColAtts, T).
table_body([\hline|T0], ColAtts, [html('<TBODY>')|T]) :-
	table_body(T0, ColAtts, T).
table_body(Body, ColAtts, [[ html('<TR VALIGN=top>'),
			     Row,
			     html('</TR>')
			  ]|Rest]) :-
	table_row(Body, 1, ColAtts, BodyRest, Row),
	table_body(BodyRest, ColAtts, Rest).

table_row([], _, _, [], []) :- !.
table_row([' '|T0], C, ColAtts, T, TH) :- !,
	table_row(T0, C, ColAtts, T, TH).
table_row(['\n'|T0], C, ColAtts, T, TH) :- !,
	table_row(T0, C, ColAtts, T, TH).
table_row([\(\, _)|Rest], _, _, Rest, []) :- !.
table_row([\(multicolumn, [{N}, {A}, {Tokens}])|R0], C, ColAtts, R,
	  [html(MC), Item|THtml]) :-
	column_alignment(A, Alignment), !,
	sformat(MC, '<TD COLSPAN=~w ALIGN=~w>', [N, Alignment]),
	translate_group(Tokens, Item),
	to_integer(N, N2),
	C2 is C + N2,
	table_cell(R0, R1, _),		% discard tokens upto &
	table_row(R1, C2, ColAtts, R, THtml).
table_row(L, C, ColAtts, R,  [html(CellHeader), Chtml, html('</TD>')|THtml]) :-
	cell_header(C, ColAtts, CellHeader),
	table_cell(L, T, Tokens),
	translate_group(Tokens, Chtml),
	C2 is C + 1,
	table_row(T, C2, ColAtts, R, THtml).

cell_header(C, ColAtts, Header) :-
	nth1(C, ColAtts, Spec),
	maplist(sgml_attribute, Spec, Attributes),
	concat_atom(['<TD'|Attributes], ' ', H0),
	concat(H0, '>', Header).

sgml_attribute(Name=Value, Att) :-
	concat_atom([Name, =, Value], Att).

to_integer(Atom, Integer) :-
	atom_codes(Atom, Chars),
	number_codes(Integer, Chars).

column_alignment(X, Alignment) :-
	atom_codes(X, Chars),
	phrase(column_alignment(Alignment), Chars).

column_alignment(A) -->
	vlines,
	calignment(A), !,
	vlines.

vlines -->
	"|", !,
	vlines.
vlines -->
	[].

calignment(left) --> "l".
calignment(center) --> "c".
calignment(right) --> "r".
calignment(left) --> [X],
	{format(user_error, 'Unknown multicolumn alignment: "~w"~n', [X])}.

table_cell([], [], []).
%table_cell([' '|T0], R, T) :- !,
%	 table_cell(T0, R, T).
table_cell([&|L], L, []) :- !.
table_cell([\(\, A)|L], [\(\, A)|L], []) :- !.
table_cell([H|T0], R, [H|T]) :-
	 table_cell(T0, R, T).

all_white_space([]).
all_white_space([' '|T]) :-
	 all_white_space(T).
all_white_space(['\n'|T]) :-
	 all_white_space(T).

clean_body([], []).
clean_body([' '|T0], T) :- !,
	 clean_body(T0, T).
clean_body(['\n'|T0], T) :- !,
	 clean_body(T0, T).
clean_body(Body, Body1) :-
	(   append(Body1, T, Body),
	    all_white_space(T)
	->  true
	).

		 /*******************************
		 *	      FIGURES		*
		 *******************************/

%	psfig_options(+PsFigOptions, -OptionList)
%
%	Translate an option-list of psfig into a list of Name(Value)
%	terms for easy further processing.

psfig_options(Text, Options) :-
	atom_codes(Text, Chars),
	phrase(psfigoptions(Options), Chars).

psfigoptions([H|T]) -->
	psfigoption(H),
	psfigoptions(T).
psfigoptions([]) --> [].

psfigoption(Term) -->
	string(NS),
	"=",
	string_without(",", VS),
	(   ","
	;   ""
	), !,
	{ atom_codes(Name, NS),
	  atom_codes(Val, VS),
	  Term =.. [Name, Val]
	}.


		 /*******************************
		 *	 FIX module/[1,2]	*
		 *******************************/


fix_predicate_reference(Ref0, Ref) :-
	 atom_codes(Ref0, Chars),
	 phrase(predref(Name, Arities), Chars),
	 member(Arity, Arities),
	 concat_atom([Name, /, Arity], Ref),
	 label(Ref, _, _), !.

predref(Name, Arities) -->
	 string(Str0),
	 "/[",
	 arityspec(Arities),
	 "]", !,
	 {atom_codes(Name, Str0)}.

arityspec(As) -->
	 integer(Low),
	 "-",
	 integer(High),
	 {findall(A, between(Low, High, A), As)}.
arityspec(As) -->
	 integer(Low),
	 "..",
	 {findall(A, between(Low, 10, A), As)}.
arityspec(A) -->
	 enumerated_arities(A).

enumerated_arities([H|T]) -->
	 integer(H), 
	 (   ","
	 ->  enumerated_arities(T)
	 ;   {T = []}
	 ).

string([]) --> [].
string([H|T]) --> [H], string(T).

		 /*******************************
		 *	     NEWCOMMAND		*
		 *******************************/

:- dynamic
	user_cmd/4.

declare_command(Name, ArgCAtom, Expanded) :-
	concat(\, CmdName, Name),
	(   tex_command_property(CmdName, _, _) % test for existence
	->  true
	;   atom_codes(ArgCAtom, ArgCChars),
	    number_codes(Args, ArgCChars),
	    make_cmd_spec(Name, Args, CmdSpec),	% \Name{+}...
	    tex_declare(CmdSpec),
	    functor(Head, CmdName, Args),
	    cmd_parms(0, Args, Head, ParmList),
	    Parms =.. [macro_arg|ParmList],
	    assert((user_cmd(Head, Mode0, Mode, HTML) :-
		           expand_macro(Parms, Expanded, Mode0, Mode, HTML)))
	).

make_cmd_spec(Name, ArgC, Spec) :-
	make_cmd_arg_spec(ArgC, ArgSpec),
	concat_atom([Name|ArgSpec], Spec).
	
make_cmd_arg_spec(0, []).
make_cmd_arg_spec(N, ['{-}'|T]) :-
	NN is N - 1,
	make_cmd_arg_spec(NN, T).
      
cmd_parms(N, N, _, []) :- !.
cmd_parms(N, A, Head, [A0|AT]) :-
	I is N + 1,
	arg(I, Head, {A0}),
	cmd_parms(I, A, Head, AT).

expand_macro(Args, Macro, Mode0, Mode, HTML) :-
	atom_codes(Macro, Chars),
	replace_args(Chars, Args, Expanded),
	tex_atom_to_tokens(Expanded, Tokens),
	translate(Tokens, Mode0, Mode, HTML).

replace_args([], _, []).
replace_args([0'#,N|T], Args, Result) :- !,
	ArgN is N - 0'0,
	arg(ArgN, Args, Val),
	atom_codes(Val, Chars),
	append(Chars, RT, Result),
	replace_args(T, Args, RT).
replace_args([C|T0], Args, [C|T]) :-
	replace_args(T0, Args, T).


		 /*******************************
		 *	      UTIL		*
		 *******************************/

%	split(+Atom, +SepString, -ListOfAtoms)

split(Atom, Sep, List) :-
	atom_codes(Atom, Chars),
	do_split(Chars, Sep, List).

do_split([], _, []).
do_split(L, Sep, [H|T]) :-
	append(Head, Rest, L),
	append(HL, Sep, Head), !,
	atom_codes(H, HL),
	do_split(Rest, Sep, T).
do_split(L, _, [A]) :-
	atom_codes(A, L).


		 /*******************************
		 *	       PS2GIF		*
		 *******************************/

option(gs,	gs).
option(res,	72).
option(device,	ppmraw).
option(tmp,	Tmp) :-
	tmp_file(ps2gif, Tmp).

ps2gif(In, Out) :-
	ps2gif(In, Out, []).

ps2gif(In, Out, _Options) :-
	absolute_file_name(In, [ access(read),
				 extensions([gif]),
				 file_errors(fail)
			       ],
			   InFile), !,
	concat_atom(['cp ', InFile, ' ', Out], Cmd),
	shell(Cmd).
ps2gif(In, Out, Options) :-
	get_option(Options, tmp(Tmp)),
	get_option(Options, res(Res0)),
	(   absolute_file_name(In, [ access(read),
				     extensions([ps, eps]),
				     file_errors(fail)
				   ],
			       InFile)
	->  true
	;   format(user_error, 'Could not find figure "~w"~n', In),
	    fail
	),
	format(user_error, 'Converting ~w to .GIF~n', [InFile]),
	get_ps_parameters(InFile, EPS, bb(X1,Y1,X2,Y2)),
	(   get_option(Options, width(W))
	->  ScaleX is W/((X2-X1)/72)
	;   ScaleX is 1
	),
	(   get_option(Options, height(H))
	->  ScaleY is H/((Y2-Y1)/72)
	;   ScaleY is 1
	),
	ResX is Res0 * ScaleX,
	ResY is Res0 * ScaleY,
	(   ResX =:= ResY
	->  Res = ResX
	;   sformat(Res, '~wx~w', [ResX, ResY])
	),
	BBX is -X1,
	BBY is -Y1,
	BBW0 = X2 - X1,
	BBH0 = Y2 - Y1,
	BBW is round(BBW0 * ResX / 72),
	BBH is round(BBH0 * ResY / 72),
	gs_command([size(BBW,BBH),tmp(Tmp),res(Res)|Options], Cmd),
	telling(Old), tell(pipe(Cmd)),
	format('~w ~w translate ', [BBX, BBY]),
	format('(~w) run ', InFile),
	(   EPS = eps
	->  format('showpage ')
	;   true
	),
	format('quit~n'),
	told, tell(Old),
	ppm2gif(Tmp, Out, Options),
	delete_file(Tmp).

ppm2gif(Tmp, Out, Options) :-
	(   get_option(Options, margin(B))
	->  aformat(Cmd,
		    'pnmcrop < ~w | pnmmargin ~w | pnmmargin -black 1 | ppmquant 192 | ppmtogif > ~w',
		    [Tmp, B, Out])
	;   aformat(Cmd, 'pnmcrop < ~w | ppmquant 192 | ppmtogif > ~w',
		    [Tmp, Out])
	),
	shell(Cmd).

gs_command(Options, Cmd) :-
	get_option(Options, gs(GS)),
	get_option(Options, res(Res)),
	get_option(Options, device(Dev)),
	get_option(Options, tmp(Tmp)),
	(   get_option(Options, size(W, H))
	->  sformat(SCmd, '-g~wx~w', [W, H])
	;   SCmd = ''
	),
	aformat(Cmd,
		'~w -q -dNOPAUSE -sDEVICE=~w ~w -r~w -sOutputFile=~w',
		[GS, Dev, SCmd, Res, Tmp]).
	
	
get_option(List, Term) :-
	memberchk(Term, List), !.
get_option(_, Term) :-
	functor(Term, Name, _),
	option(Name, Def), !,
	arg(1, Term, Def).
	
aformat(Atom, Fmt, Args) :-
	sformat(Str, Fmt, Args),
	string_to_atom(Str, Atom).


		 /*******************************
		 *	      OUTPUT		*
		 *******************************/


:- dynamic
	pending_par/0.

implicit_par(html('<H1>')).
implicit_par(html('<H2>')).
implicit_par(html('<H3>')).
implicit_par(html('<H4>')).
implicit_par(html('<PRE>')).
implicit_par(html('<XMP>')).
implicit_par(html('<DL>')).
implicit_par(html('<TABLE>')).
%implicit_par(html('<CENTER>')).
implicit_par(html('<BLOCKQUOTE>')).

no_par_in('<DL>').			% delete pars here
%no_par_in('<UL>').
%no_par_in('<OL>').


write_html([]) :- !.			% Unpack lists
write_html([H|T]) :- !,
	write_html(H),
	write_html(T).
write_html('\n') :-
	pending_par, !.
write_html(' ') :-
	pending_par, !.
write_html(html('<P>')) :- !,
	(   pending_par
	->  true
	;   assert(pending_par)
	).
write_html(html(Cmd)) :-
	no_par_in(Cmd), !,
	(   pending_par
	->  true
	;   assert(pending_par)		% Say we have one, do others
					% are suppressed
	),
	cmd_layout(Cmd, Pre, Post), !,
	put_html_token(html(Cmd, Pre, Post)).
write_html(Token) :-
	retract(pending_par), !,
	(   implicit_par(Token)
	->  true
	;   cmd_layout('<P>', Pre, Post),
	    put_html_token(html('<P>', Pre, Post))
	),
	write_html(Token).
write_html(html(Cmd)) :-			% HTML commands
	cmd_layout(Cmd, Pre, Post), !,
	put_html_token(html(Cmd, Pre, Post)).
write_html(ref(Label)) :- !,			% References and labels
	(   label(Label, _, Ref)
	->  write_html(Ref)
	;   write_html('??'),
	    format(user_error, 'No label for ref "~w"~n', [Label])
	).
write_html(label(Label, Text, _)) :- !,
	(   in_anchor
	->  write_html(Text)
	;   sformat(Anchor, '<A NAME="~w">', [Label]),
	    asserta(in_anchor),
	    write_html([html(Anchor), Text, html('</A>')]),
	    retractall(in_anchor)
	).
write_html(body_link(Link)) :- !,
	(   translate_ref(Link, Ref, Type)
	->  sformat(Anchor, '<A HREF="~w">', [Ref]),
	    capitalise_atom(Type, Text),
	    (	link_image(Type, Image)
	    ->	sformat(Img, '<IMG SRC="~w" BORDER=0 ALT="~w">', [Image, Text]),
		Label = html(Img)
	    ;	Label = Text
	    ),
	    write_html([html(Anchor), Label, html('</A>')]),
	    nl_html
	;   true
	).
write_html(link(Link)) :- !,
	(   translate_ref(Link, Ref, Type)
	->  sformat(Html, '<LINK REL=~w HREF="~w">', [Type, Ref]),
	    write_html(html(Html)),
	    nl_html
	;   true
	).
write_html(iflref(fileof(Label), Text)) :- !,
	(   label(Label, _, _)
	->  write_html(lref(fileof(Label), Text))
	;   true
	).
write_html(iflref(Label, Text)) :- !,
	(   label(Label, _, _)
	->  write_html(lref(Label, Text))
	;   true
	).
write_html(lref(fileof(Label), Text)) :-
	(   label(Label, File, _)
	->  sformat(Anchor, '<A HREF="~w.html">', [File]),
	    write_html([html(Anchor), Text, html('</A>')])
	;   write_html(lref(Label, Text))
	).
write_html(lref(Label, Text)) :-
	label(Label, File, _), !,
	(   in_anchor
	->  macro_expand(Text, Expanded),
	    write_html(Expanded)
	;   asserta(in_anchor),
	    (   onefile(false)
	    ->  sformat(Anchor, '<A HREF="~w.html#~w">', [File, Label])
	    ;   sformat(Anchor, '<A HREF="#~w">', [Label])
	    ),
	    write_html([html(Anchor), Text, html('</A>')]),
	    retractall(in_anchor)
	).
write_html(lref(Label, Text)) :-
	fix_predicate_reference(Label, FixedLabel), !,
	write_html(lref(FixedLabel, Text)).
write_html(lref(Label, Text)) :- !,
	format(user_error, 'No label for reference "~w"~n', [Label]),
	macro_expand(#b(Text), Expanded),
	write_html(Expanded).
write_html(lforw(Label, Text)) :- !,
	concat('back-to-', Label, BackName),
	(   label(Label, File, _)
	->  sformat(Anchor, '<A NAME=~w HREF="~w.html#~w">',
		    [BackName, File, Label]), 
	    write_html([html(Anchor), Text, html('</A>')])
	;   format(user_error, 'No target for forward link "~w"~n', [Label])
	).
write_html(lback(Label, Text)) :- !,
	concat('back-to-', Label, BackName),
	(   label(BackName, File, _)
	->  sformat(Anchor, '<A NAME=~w HREF="~w.html#~w">',
		    [Label, File, BackName]), 
	    write_html([html(Anchor), Text, html('</A>')])
	;   format(user_error, 'No target for backward link "~w"~n', [Label])
	).
write_html(cite(Key)) :- !,
	(   cite(Key, Cite)
	->  write_html(Cite)
	;   write_html([nospace('['), Key, nospace(']')]),
	    format(user_error, 'No bibliography entry for "~w"~n', Key)
	).
write_html(yearcite(Key)) :- !,
	(   cite(Key, CiteList)
	->  (   last(CiteList, Year),
		name(Year, Chars),
		name(YearInt, Chars),
		integer(YearInt)
	    ->  write_html(Year)
	    ;   format(user_error,
		       'No year for bibliography entry "~w"~n', Key),
	        write_html(cite(Key))
	    )
	;   write_html(cite(Key))
	).
write_html(tableofcontents(document)) :- !,
	tableofcontents(Table),
	macro_expand(Table, HTML),
	write_html(HTML).
write_html(tableofcontents(section(Tag))) :- !,
	subsection_index(Tag, Table),
	macro_expand(Table, HTML),
	write_html(HTML).
write_html(tell(Base)) :- !,
	close_output,
	open_output(Base).
write_html(H) :-
	put_html_token(H), !.
write_html(_).
	
nl_html :-
	write_html(verb('\n')).

%	translate_ref(+Label, -Anchor, -TextLabel)

translate_ref(next, Anchor, next) :-
	current_html_output(Current),
	next_file(Current, Next),
	concat(Next, '.html', Anchor).
translate_ref(previous, Anchor, previous) :-
	current_html_output(Current),
	next_file(Prev, Current),
	concat(Prev, '.html', Anchor).
translate_ref(up(Label), Anchor, up) :-
	label(Label, File, _),
	concat(File, '.html', Anchor).
translate_ref(home, Anchor, home) :-
	html_file_base(Home),
	\+ current_html_output(Home),
	onefile(false),
	concat(Home, '.html', Anchor).
translate_ref(contents, Anchor, contents) :-
	label('document-contents', File, _),
	concat(File, '.html', Anchor).
translate_ref(index, Anchor, index) :-
	label('document-index', File, _),
	concat(File, '.html', Anchor).
translate_ref(summary, Anchor, summary) :-
	label('sec:summary', File, _),
	concat(File, '.html', Anchor).


cmd_layout('<P>',    2, 0).
cmd_layout('<DL>',   2, 1).
cmd_layout('</DL>',  1, 2). 
cmd_layout('<DD>',   0, 1). 
cmd_layout('<H1>',   2, 0). 
cmd_layout('<H2>',   2, 0). 
cmd_layout('<H3>',   2, 0). 
cmd_layout('<H4>',   2, 0). 
cmd_layout('<H5>',   2, 0). 
cmd_layout('<H6>',   2, 0). 
cmd_layout('</H1>',  0, 2). 
cmd_layout('</H2>',  0, 2). 
cmd_layout('</H3>',  0, 2). 
cmd_layout('</H4>',  0, 2). 
cmd_layout('</H5>',  0, 2). 
cmd_layout('</H6>',  0, 2). 
cmd_layout('<HR>',   1, 1). 
cmd_layout('<BR>',   0, 1). 
cmd_layout('<LI>', 	 1, 0).
cmd_layout('<DT>', 	 1, 0).
cmd_layout('<UL>', 	 1, 1).
cmd_layout('</UL>', 	 1, 1).
cmd_layout('<TR>',       1, 0). 
cmd_layout('</TR>',      0, 1). 
cmd_layout('<TBODY>',    1, 1). 
cmd_layout('<THEAD>',    1, 1). 
cmd_layout('</TABLE>',   0, 2). 
cmd_layout('<LISTING>',	 2, 0). 
cmd_layout('</LISTING>', 0, 2). 
cmd_layout('<PRE>',	 1, 1). 
cmd_layout('</PRE>', 	 1, 1). 
cmd_layout('<XMP>',	 2, 0). 
cmd_layout('</XMP>', 	 0, 2). 
cmd_layout('<HEAD>',	 1, 1). 
cmd_layout('</HEAD>',	 1, 1). 
cmd_layout('<CENTER>',	 1, 1). 
cmd_layout('</CENTER>',	 1, 1). 
cmd_layout('<BODY>',	 2, 1). 
cmd_layout('</BODY>',	 1, 1). 
cmd_layout('</HEAD>',	 0, 1). 
cmd_layout('<HTML>',	 0, 1). 
cmd_layout('</HTML>',	 1, 1). 
cmd_layout('<BLOCKQUOTE>',	1, 0). 
cmd_layout('</BLOCKQUOTE>',	0, 1). 
cmd_layout(Cmd,		 	1, 1) :-
	concat('<TABLE', _, Cmd).

:- initialization
   read_tex_inputs.


