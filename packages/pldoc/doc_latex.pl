/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

:- module(pldoc_latex,
	  [ latex_for_file/3,		% +FileSpec, +Out, +Options
	    latex_for_wiki_file/3	% +FileSpec, +Out, +Options
	  ]).
:- use_module(library(readutil)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(doc_html, [doc_file_objects/5]).
:- use_module(doc_wiki).
:- use_module(doc_process).
:- use_module(doc_modes).

/** <module> PlDoc LaTeX backend

This  module  translates  the  Herbrand   term  from  the  documentation
extracting module doc_wiki.pl into LaTeX using the pl.sty style file.

@tbd	Totally incomplete; finish
@author	Jan Wielemaker
*/

%%	latex_for_file(+File, +Out, +Options) is det.
%
%	Generate a LaTeX description of all commented predicates in
%	File.   Options:
%	
%		* stand_alone(+Bool)
%		If =true= (default), create a document that can be run
%		through LaTeX.  If =false=, produce a document to be
%		included in another LaTeX document.
%		
%		* public_only(+Bool)
%		If =true= (default), only emit documentation for
%		exported predicates.

latex_for_file(FileSpec, Out, Options) :-
	absolute_file_name(FileSpec,
			   [ file_type(prolog),
			     access(read)
			   ],
			   File),
	file_base_name(File, _Base),
	doc_file_objects(FileSpec, File, Objects, FileOptions, Options),
	phrase(latex([ \file_header(File, FileOptions)
		     | \objects(Objects, FileOptions)
		     ]),
	       Tokens),
	latex_header(Out, Options),
	print_latex_tokens(Tokens, Out),
	latex_footer(Out, Options).


%%	latex_for_wiki_file(+File, +Out, +Options)
%
%	Generate a LaTeX translation of a Wiki file.

latex_for_wiki_file(FileSpec, Out, _Options) :-
	absolute_file_name(FileSpec, File,
			   [ access(read)
			   ]),
	read_file_to_codes(File, String, []),
	b_setval(pldoc_file, File),
	call_cleanup((wiki_string_to_dom(String, [], DOM),
		      phrase(latex(DOM), Tokens),
		      print_latex_tokens(Tokens, Out)
		     ),
		     nb_delete(pldoc_file)).


		 /*******************************
		 *	 LATEX PRODUCTION	*
		 *******************************/

latex([]) --> !,
	[].
latex(Atomic) -->
	{ atomic(Atomic), !,
	  findall(x, sub_atom(Atomic, _, _, _, '\n'), Xs),
	  length(Xs, Lines)
	},
	(   {Lines == 0}
	->  [ Atomic ]
	;   [ nl(Lines) ]
	).
latex([H|T]) -->
	(   latex(H)
	->  latex(T)
	;   { print_message(error, latex(failed(H))) },
	    latex(T)
	).

% high level commands
latex(h1(_Class, Content)) -->
	latex(cmd(section(Content))).
latex(h2(_Class, Content)) -->
	latex(cmd(subsection(Content))).
latex(h3(_Class, Content)) -->
	latex(cmd(subsubsection(Content))).
latex(p(Content)) -->
	[ nl_exact(2) ],
	latex(Content).
latex(a(Attrs, Content)) -->
	{ attribute(href(HREF), Attrs) },
	(   {HREF == Content}
	->  latex(cmd(url(HREF)))
	;   latex(cmd(url(opt(Content), HREF)))
	).
latex(code(CodeList)) -->
	{ is_list(CodeList), !,
	  concat_atom(CodeList, Atom)
	},
	[ verb(Atom) ].
latex(code(Code)) -->
	{ identifier(Code) }, !,
	latex(cmd(const(Code))).
latex(code(Code)) -->
	[ verb(Code) ].
latex(b(Code)) -->
	latex(cmd(textbf(Code))).
latex(i(Code)) -->
	latex(cmd(textit(Code))).
latex(var(Var)) -->
	latex(cmd(arg(Var))).
latex(pre(_Class, Code)) -->
	[ code(Code) ].
latex(ul(Content)) -->
	latex(cmd(begin(itemize))),
	latex(Content),
	latex(cmd(end(itemize))).
latex(li(Content)) -->
	latex(cmd(item)),
	latex(Content).
latex(dl(_, Content)) -->
	latex(cmd(begin(description))),
	latex(Content),
	latex(cmd(end(description))).
latex(dd(_, Content)) -->
	latex(Content).
latex(dd(Content)) -->
	latex(Content).
latex(dt(class=term, \term(Term, Bindings))) -->
	{ bind_vars(Bindings),
	  Term =.. [Functor|Args]
	}, !,
	latex(cmd(termitem(Functor, \pred_args(Args, 1)))).
latex(\Cmd, List, Tail) :-
	call(Cmd, List, Tail).

% low level commands
latex(latex(Text)) -->
	[ latex(Text) ].
latex(cmd(Term)) -->
	{ Term =.. [Cmd|Args] },
	indent(Cmd),
	[ cmd(Cmd) ],
	latex_arguments(Args),
	outdent(Cmd).

indent(begin) --> !,         [ nl(1) ].
indent(end) --> !,           [ nl_exact(1) ].
indent(section) --> !,       [ nl(2) ].
indent(subsection) --> !,    [ nl(2) ].
indent(subsubsection) --> !, [ nl(2) ].
indent(item) --> !,          [ nl(1), indent(4) ].
indent(tag) --> !,           [ nl(1), indent(4) ].
indent(term_item) --> !,     [ nl(1), indent(4) ].
indent(predicate) --> !,     [ nl(1), indent(4) ].
indent(dcg) --> !,           [ nl(1), indent(4) ].
indent(infixop) --> !,       [ nl(1), indent(4) ].
indent(prefixop) --> !,      [ nl(1), indent(4) ].
indent(postfixop) --> !,     [ nl(1), indent(4) ].
indent(_) --> [].

outdent(begin) --> !,         [ nl_exact(1) ].
outdent(end) --> !,           [ nl(1) ].
outdent(item) --> !,	      [ ' ' ].
outdent(tag) --> !,           [ nl(1) ].
outdent(termitem) --> !,      [ nl(1) ].
outdent(section) --> !,       [ nl(2) ].
outdent(subsection) --> !,    [ nl(2) ].
outdent(subsubsection) --> !, [ nl(2) ].
outdent(predicate) --> !,     [ nl(1) ].
outdent(dcg) --> !,           [ nl(1) ].
outdent(infixop) --> !,       [ nl(1) ].
outdent(prefixop) --> !,      [ nl(1) ].
outdent(postfixop) --> !,     [ nl(1) ].
outdent(_) --> [].

latex_arguments([]) --> [].
latex_arguments([opt(H)|T]) --> !,
	[ '[' ],
	latex(H),
	[ ']' ],
	latex_arguments(T).
latex_arguments([H|T]) -->
	[ curl(open) ],
	latex(H),
	[ curl(close) ],
	latex_arguments(T).


attribute(Att, Attrs) :-
	is_list(Attrs), !,
	option(Att, Attrs).
attribute(Att, One) :-
	option(Att, [One]).


		 /*******************************
		 *	   \ COMMANDS		*
		 *******************************/
file(File) -->
	latex(cmd(file(File))).

predref(Name/Arity) -->
	latex(cmd(predref(Name, Arity))).

%%	tags(+Tags:list(Tag)) is det.
%
%	Emit tag list.  

tags([\params(Params)|Rest]) --> !,
	params(Params),
	tags_list(Rest).
tags(List) -->
	tags_list(List).

tags_list([]) -->
	[].
tags_list(List) -->
	[ nl(2) ],
	latex(cmd(begin(description))),
	latex(List),
	latex(cmd(end(description))),
	[ nl(2) ].

%%	tag(+Tag, +Value)// is det.
%
%	Called from \tag(Name, Value) terms produced by doc_wiki.pl.

tag(Tag, Value) -->
	{   pldoc_html:tag_title(Tag, Title)
	->  true
	;   Title = Tag
	},
	{   pldoc_html:tag_class(Tag, Class)
	->  true
	;   Class = tag
	},
	latex([cmd(tag(Title)), Value]).


%%	params(+Params:list) is det.
%
%	Called from \params(List) created by   doc_wiki.pl.  Params is a
%	list of param(Name, Descr).

params(Params) -->
	latex([ cmd(begin(parameters)),
		\param_list(Params),
		cmd(end(parameters))
	      ]).

param_list([]) -->
	[].
param_list([H|T]) -->
	param(H),
	param_list(T).

param(param(Name,Descr)) -->
	[ nl(1) ],
	latex(cmd(arg(Name))), [ latex(' & ') ],
	latex(Descr), [latex(' \\\\')].

%%	file_header(+File, +Options)// is det.
%
%	Create the file header.

file_header(File, Options) -->
	{ memberchk(file(Title, Comment), Options), !,
	  file_base_name(File, Base)
	},
	file_title([Base, ' -- ', Title], File, Options),
	{ is_structured_comment(Comment, Prefixes),
	  indented_lines(Comment, Prefixes, Lines),
	  section_comment_header(Lines, _Header, Lines1),
	  wiki_lines_to_dom(Lines1, [], DOM)
	},
	latex(DOM).
file_header(File, Options) -->
	{ file_base_name(File, Base)
	},
	file_title([Base], File, Options).


%%	file_title(+Title:list, +File, +Options)// is det
%
%	Emit the file-header and manipulation buttons.

file_title(Title, _File, _Options) -->
	latex(cmd(section(Title))).


%%	objects(+Objects:list, +Options)// is det.
%
%	Emit the documentation body.

objects(Objects, Options) -->
	objects(Objects, [body], Options).

objects([], Mode, _) -->
	pop_mode(body, Mode, _).
objects([Obj|T], Mode, Options) -->
	object(Obj, Mode, Mode1, Options),
	objects(T, Mode1, Options).

object(doc(Obj,Pos,Comment), Mode0, Mode, Options) --> !,
	object(Obj, Pos, Comment, Mode0, Mode, Options).
object(Obj, Mode0, Mode, Options) -->
	{ doc_comment(Obj, Pos, _Summary, Comment)
	}, !,
	object(Obj, Pos, Comment, Mode0, Mode, Options).

object(Obj, Pos, Comment, Mode0, Mode, Options) -->
	{ pldoc_html:pi(Obj), !,
	  is_structured_comment(Comment, Prefixes),
	  indented_lines(Comment, Prefixes, Lines),
	  process_modes(Lines, Pos, Modes, Args, Lines1),
	  (   pldoc_html:private(Obj, Options)
	  ->  Class = privdef		% private definition
	  ;   Class = pubdef		% public definition
	  ),
	  (   Obj = Module:_
	  ->  POptions = [module(Module)|Options]
	  ;   POptions = Options
	  ),
	  DOM = [\pred_dt(Modes, Class, POptions), dd(class=defbody, DOM1)],
	  wiki_lines_to_dom(Lines1, Args, DOM0),
	  strip_leading_par(DOM0, DOM1)
	},
	need_mode(description, Mode0, Mode),
	latex(DOM).
object([Obj|_Same], Pos, Comment, Mode0, Mode, Options) --> !,
	object(Obj, Pos, Comment, Mode0, Mode, Options).
object(Obj, _Pos, _Comment, Mode, Mode, _Options) -->
	{ debug(pldoc, 'Skipped ~p', [Obj]) },
	[].
	

%%	need_mode(+Mode:atom, +Stack:list, -NewStack:list)// is det.
%
%	While predicates are part of a   description  list, sections are
%	not and we therefore  need  to   insert  <dl>...</dl>  into  the
%	output. We do so by demanding  an outer environment and push/pop
%	the required elements.

need_mode(Mode, Stack, Stack) -->
	{ Stack = [Mode|_] }, !,
	[].
need_mode(Mode, Stack, Rest) -->
	{ memberchk(Mode, Stack)
	}, !,
	pop_mode(Mode, Stack, Rest).	
need_mode(Mode, Stack, [Mode|Stack]) --> !,
	latex(cmd(begin(Mode))).

pop_mode(Mode, Stack, Stack) -->
	{ Stack = [Mode|_] }, !,
	[].
pop_mode(Mode, [H|Rest0], Rest) -->
	latex(cmd(end(H))),
	pop_mode(Mode, Rest0, Rest).


%%	pred_dt(+Modes, +Class, Options)// is det.
%
%	Emit the \predicate{}{}{} header.
%	
%	@param Modes	List as returned by process_modes/5.
%	
%	@tbd	Determinism

pred_dt(Modes, Class, Options) -->
	[nl(2)],
	pred_dt(Modes, [], _Done, [class(Class)|Options]).

pred_dt([], Done, Done, _) -->
	[].
pred_dt([H|T], Done0, Done, Options) -->
	pred_mode(H, Done0, Done1, Options),
	(   {T == []}
	->  []
	;   latex(cmd(nodescription)),
	    pred_dt(T, Done1, Done, Options)
	).

pred_mode(mode(Head,Vars), Done0, Done, Options) --> !,
	{ bind_vars(Head, Vars) },
	pred_mode(Head, Done0, Done, Options).
pred_mode(Head is _Det, Done0, Done, Options) --> !,
	anchored_pred_head(Head, Done0, Done, Options).
pred_mode(Head, Done0, Done, Options) -->
	anchored_pred_head(Head, Done0, Done, Options).

bind_vars(Term, Bindings) :-
	bind_vars(Bindings),
	anon_vars(Term).

bind_vars([]).
bind_vars([Name=Var|T]) :-
	Var = '$VAR'(Name),
	bind_vars(T).

%%	anon_vars(+Term) is det.
%
%	Bind remaining variables in Term to '$VAR'('_'), so they are
%	printed as '_'.

anon_vars(Var) :-
	var(Var), !,
	Var = '$VAR'('_').
anon_vars(Term) :-
	compound(Term), !,
	Term =.. [_|Args],
	maplist(anon_vars, Args).
anon_vars(_).


anchored_pred_head(Head, Done0, Done, Options) -->
	{ pldoc_html:anchor_name(Head, PI, _Name)
	},
	(   { memberchk(PI, Done0) }
	->  { Done = Done0 },
	    pred_head(Head, Options)
	;   { Done = [PI|Done0] }
	),
	pred_head(Head, Options).


%%	pred_head(+Term, Options) is det.
%
%	Emit a predicate head. The functor is  typeset as a =span= using
%	class =pred= and the arguments and =var= using class =arglist=.

pred_head(//(Head), _Options) --> !,
	{ Head =.. [Functor|Args],
	  length(Args, Arity)
	},
	latex(cmd(dcg(Functor, Arity, \pred_args(Args, 1)))).
pred_head(Head, _Options) -->
	{ atom(Head) }, !,
	latex(cmd(predicate(Head, 0, ''))).
pred_head(Head, _Options) -->			% Infix operators
	{ Head =.. [Functor,Left,Right],
	  current_op(_,Type,Functor),
	  op_type(Type, infix), !
	},
	latex(cmd(infixop(Functor, \pred_arg(Left, 1), \pred_arg(Right, 2)))).
pred_head(Head, _Options) -->			% Prefix operators
	{ Head =.. [Functor,Arg],
	  current_op(_,Type,Functor),
	  op_type(Type, prefix), !
	},
	latex(cmd(prefixop(Functor, \pred_arg(Arg, 1)))).
pred_head(Head, _Options) -->			% Postfix operators
	{ Head =.. [Functor,Arg],
	  current_op(_,Type,Functor),
	  op_type(Type, postfix), !
	},
	latex(cmd(postfixop(Functor, \pred_arg(Arg, 1)))).
pred_head(Head, _Options) -->			% Plain terms
	{ Head =.. [Functor|Args],
	  length(Args, Arity)
	},
	latex(cmd(predicate(Functor, Arity, \pred_args(Args, 1)))).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xf,  postfix).
op_type(yf,  postfix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).


pred_args([], _) -->
	[].
pred_args([H|T], I) -->
	pred_arg(H, I),
	(   {T==[]}
	->  []
	;   latex(', '),
	    { I2 is I + 1 },
	    pred_args(T, I2)
	).

pred_arg(Var, I) -->
	{ var(Var) }, !,
	latex(['Arg', I]).
pred_arg(...(Term), I) --> !,
	pred_arg(Term, I),
	latex(cmd(ldots)).
pred_arg(Term, I) -->
	{ Term =.. [Ind,Arg],
	  mode_indicator(Ind)
	}, !,
	latex([Ind, \pred_arg(Arg, I)]).
pred_arg(Arg:Type, _) --> !,
	latex([\argname(Arg), :, \argtype(Type)]).
pred_arg(Arg, _) -->
	argname(Arg).

argname('$VAR'(Name)) --> !,
	latex(Name).
argname(Name) --> !,
	latex(Name).

argtype(Term) -->
	{ format(string(S), '~W',
		 [ Term,
		   [ quoted(true),
		     numbervars(true)
		   ]
		 ]) },
	latex(S).

%pred_det(unknown) -->
%	[].
%pred_det(Det) -->
%	html([' is ', b(class=det, Det)]).


%%	term(+Term, +Bindings)// is det.
%
%	Process the \term element as produced by doc_wiki.pl.
%	
%	@tbd	Properly merge with pred_head//1

term(Atom, []) -->
	{ atomic(Atom) }, !,
	latex(Atom).
term(Term, Bindings) -->
	{ bind_vars(Bindings),
	  Term =.. [Functor|Args]
	}, !,
	latex(cmd(term(Functor, \pred_args(Args, 1)))).


		 /*******************************
		 *	    PRINT TOKENS	*
		 *******************************/

%%	print_latex_tokens(+Tokens, +Out)
%
%	Print primitive LaTeX tokens to Output

print_latex_tokens([], _).
print_latex_tokens([nl(N)|T0], Out) :- !,
	max_nl(T0, T, N, NL),
	nl(Out, NL),
	print_latex_tokens(T, Out).
print_latex_tokens([nl_exact(N)|T0], Out) :- !,
	nl_exact(T0, T,N, NL),
	nl(Out, NL),
	print_latex_tokens(T, Out).
print_latex_tokens([H|T], Out) :-
	print_latex_token(H, Out),
	print_latex_tokens(T, Out).

print_latex_token(cmd(Cmd), Out) :- !,
	format(Out, '\\~w', [Cmd]).
print_latex_token(curl(open), Out) :- !,
	format(Out, '{', []).
print_latex_token(curl(close), Out) :- !,
	format(Out, '}', []).
print_latex_token(indent(N), Out) :- !,
	format(Out, '~t~*|', [N]).
print_latex_token(nl(N), Out) :- !,
	format(Out, '~N', []),
	forall(between(2,N,_), nl(Out)).
print_latex_token(verb(Verb), Out) :-
	is_list(Verb), Verb \== [], !,
	concat_atom(Verb, Atom),
	print_latex_token(verb(Atom), Out).
print_latex_token(verb(Verb), Out) :- !,
	(   member(C, [$,'|',@,=,'"',^,!]),
	    \+ sub_atom(Verb, _, _, _, C)
	->  format(Out, '\\verb~w~w~w', [C,Verb,C])
	;   assertion(fail)
	).
print_latex_token(code(Code), Out) :- !,
	format(Out, '~N\\begin{code}~n', []),
	format(Out, '~w', [Code]),
	format(Out, '~N\\end{code}~n', []).
print_latex_token(latex(Code), Out) :- !,
	write(Out, Code).
print_latex_token(Rest, Out) :-
	(   atomic(Rest)
	->  print_latex(Out, Rest)
	;   %type_error(latex_token, Rest)
	    write(Out, Rest)
	).

%%	print_latex(+Out, +Text:atomic) is det.
%
%	Print Text, such that it comes out as normal LaTeX text.

print_latex(Out, String) :-
	atom_chars(String, Chars),
	print_chars(Chars, Out).

print_chars([], _).
print_chars([H|T], Out) :-
	print_char(H, Out),
	print_chars(T, Out).


%%	max_nl(T0, T, M0, M)
%
%	Remove leading sequence of nl(N) and return the maximum of it.

max_nl([nl(M1)|T0], T, M0, M) :- !,
	M2 is max(M1, M0),
	max_nl(T0, T, M2, M).
max_nl([nl_exact(M1)|T0], T, _, M) :- !,
	nl_exact(T0, T, M1, M).
max_nl(T, T, M, M).

nl_exact([nl(_)|T0], T, M0, M) :- !,
	max_nl(T0, T, M0, M).
nl_exact([nl_exact(M1)|T0], T, M0, M) :- !,
	M2 is max(M1, M0),
	max_nl(T0, T, M2, M).
nl_exact(T, T, M, M).


nl(Out, N) :-
	forall(between(1, N, _), nl(Out)).


print_char('<', Out) :- !, write(Out, '$<$').
print_char('>', Out) :- !, write(Out, '$>$').
print_char('{', Out) :- !, write(Out, '\\{').
print_char('}', Out) :- !, write(Out, '\\}').
print_char('$', Out) :- !, write(Out, '\\$').
print_char('#', Out) :- !, write(Out, '\\#').
print_char('\\',Out) :- !, write(Out, '\\bsl{}').
print_char(C,   Out) :- put_char(Out, C).


%%	identifier(+Atom) is semidet.
%
%	True if Atom is (lower, alnum*).

identifier(Atom) :-
	atom_chars(Atom, [C0|Chars]),
	char_type(C0, lower),
	all_chartype(Chars, alnum).

all_chartype([], _).
all_chartype([H|T], Type) :-
	char_type(H, Type),
	all_chartype(T, Type).


		 /*******************************
		 *	   HEADER/FOOTER	*
		 *******************************/

latex_header(Out, Options) :-
	(   option(stand_alone(true), Options, true)
	->  forall(header(Line), format(Out, '~w~n', [Line]))
	;   true
	).

latex_footer(Out, Options) :-
	(   option(stand_alone(true), Options, true)
	->  forall(footer(Line), format(Out, '~w~n', [Line]))
	;   true
	).

header('\\documentclass[11pt]{article}').
header('\\usepackage{times}').
header('\\usepackage{pl}').
header('\\sloppy').
header('\\makeindex').
header('').
header('\\begin{document}').

footer('').
footer('\\printindex').
footer('\\end{document}').
