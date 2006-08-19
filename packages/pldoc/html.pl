/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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

:- module(pldoc_html,
	  [ doc_for_file/3,		% +FileSpec, +Out, +Options
	    doc_write_html/3,		% +Stream, +Title, +Term
	    				% Support doc_index
	    doc_page_dom/3,		% +Title, +Body, -DOM
	    print_html_head/1,		% +Stream
	    predref/3,			% +PI, //
	    module_info/3,		% +File, +Options0, -Options
	    doc_hide_private/3,		% +Doc0, -Doc, +Options
	    edit_button/4,		% +File, +Options, //

	    private/2,			% +Obj, +Options
	    tags/3,			% +Tags, //
	    file_header/4,		% +File, +Options, //
	    objects/4			% +Objects, +Options, //
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(url)).
:- use_module(library('http/html_write')).
:- use_module(process).
:- use_module(modes).
:- use_module(wiki).
:- use_module(doc_search).

/** <module> PlDoc HTML backend

This module translates the Herbrand term from the documentation
extracting module wiki.pl into HTML+CSS.
*/

		 /*******************************
		 *	 FILE PROCESSING	*
		 *******************************/

%%	doc_for_file(+File, +Out, +Options) is det
%
%	Write documentation for File to out in HTML.  Options:
%	
%		* public_only(+Bool)
%		If =true= (default), only emit documentation for
%		exported predicates.
%	
%		* edit(Bool)
%		If =true=, provide edit buttons. Default, these buttons
%		are suppressed.
%		
%	@param File	Prolog file specification.
%	@param Out	Output stream

doc_for_file(FileSpec, Out, Options) :-
	absolute_file_name(FileSpec,
			   [ file_type(prolog),
			     access(read)
			   ],
			   File),
	file_base_name(File, Base),
	Title = Base,
	doc_page_dom(Title, \prolog_file(FileSpec, Options), DOM),
	phrase(html(DOM), Tokens),
	print_html_head(Out),
	print_html(Out, Tokens).

prolog_file(FileSpec, Options) -->
	{ absolute_file_name(FileSpec,
			     [ file_type(prolog),
			       access(read)
			     ],
			     File),
	  Pos = File:_Line,
	  findall(doc(Obj,Pos,Comment),
		  doc_comment(Obj, Pos, _, Comment), Objs0),
	  module_info(File, ModuleOptions, Options),
	  file_info(Objs0, Objs1, FileOptions, ModuleOptions),
	  doc_hide_private(Objs1, Objs, ModuleOptions),
	  b_setval(pldoc_file, File)	% TBD: delete?
	},
	html([ \links(File, FileOptions),
	       \file_header(File, FileOptions)
	     | \objects(Objs, FileOptions)
	     ]),
	undocumented(Objs, FileOptions).
	  
%%	module_info(+File, -ModuleOptions, +OtherOptions) is det.
%
%	Add options module(Name),  public(Exports)   to  OtherOptions if
%	File is a module file.

module_info(File, [module(Module), public(Exports)|Options], Options) :-
	current_module(Module, File), !,
	export_list(Module, Public),
	maplist(head_to_pi, Public, Exports).
module_info(_, Options, Options).

head_to_pi(M:Head, M:PI) :- !,
	head_to_pi(Head, PI).
head_to_pi(Head, Name/Arity) :-
	functor(Head, Name, Arity).
	

%%	doc_hide_private(+Objs, +Public, +Options)
%
%	Remove the private objects from Objs according to Options.

doc_hide_private(Objs, Objs, Options) :-
	\+ option(public(_), Options), !.
doc_hide_private(Objs, Objs, Options) :-
	option(public_only(false), Options, true), !.
doc_hide_private(Objs0, Objs, Options) :-
	hide_private(Objs0, Objs, Options).

hide_private([], [], _).
hide_private([H|T0], T, Options) :-
	arg(1, H, Obj),
	private(Obj, Options), !,
	hide_private(T0, T, Options).
hide_private([H|T0], [H|T], Options) :-
	hide_private(T0, T, Options).


%%	private(+Obj, +Options) is semidet.
%
%	True if Obj is not  exported   from  Options. This means Options
%	defined a module and Obj is  not   member  of the exports of the
%	module.

private(Module:PI, Options) :-
	option(module(Module), Options),
	option(public(Public), Options), !,
	\+ ( member(PI2, Public) ,
	     eq_pi(PI, PI2)
	   ).
private(Module:PI, _Options) :-
	export_list(Module, Public),
	\+ ( member(Head, Public),
	     head_to_pi(Head, PI2),
	     eq_pi(PI, PI2)
	   ).

%%	file_info(+Comments, -RestComment, -FileOptions, +OtherOptions) is det.
%
%	Add options file(Title, Comment) to OtherOptions if available.

file_info(Comments, RestComments, [file(Title, Comment)|Opts], Opts) :-
	select(doc(_:module(Title),_,Comment), Comments, RestComments), !.
file_info(Comments, Comments, Opts, Opts).


%%	links(+File, +Options) is det.
%
%	Provide overview links and search facilities.

links(_File, _Options) -->
	html(div(class(navhdr),
		 [ div(style('float:right'),
		       [ \search_form
		       ]),
		   a(href('index.html'),
		     img([ border=0,
			   src('/up.gif')
			 ]))
		 ])).


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
	html(DOM).
file_header(File, Options) -->
	{ file_base_name(File, Base)
	},
	file_title([Base], File, Options).


%%	file_title(+Title:list, +File, +Options)// is det
%
%	Emit the file-header and manipulation buttons.

file_title(Title, File, Options) -->
	{ file_base_name(File, Base)
	},
	html(h1(class=file,
		[ div(style('float:right'),
		      [ \reload_button(Base, Options),
			\zoom_button(Base, Options),
			\edit_button(File, Options)
		      ])
		| Title
		])).


%%	reload_button(+File)// is det.
%
%	Create a button for  reloading  the   sources  and  updating the
%	documentation page.

reload_button(Base, Options) -->
	{ option(public_only(Public), Options, true),
	  format(string(HREF), '~w?reload=true&public_only=~w', 
		 [Base, Public])
	},
	html(a(href=HREF,
	       img([ %class(icon),
		     border=0,
		     height=24,
		     style('padding-top:4px'),
		     src('/reload.gif')
		   ]))).


%%	edit_button(+File, +Options)// is det.
%
%	Create an edit button  for  File.   If  the  button  is clicked,
%	JavaScript sends a message to the   server without modifying the
%	current page.  JavaScript code is in the file pldoc.js.

edit_button(File, Options) -->
	{ option(edit(true), Options), !,
	  option(button_height(H), Options, 24),
	  www_form_encode(File, Enc),
	  format(string(HREF), '/edit?file=~w', [Enc]),
	  format(string(OnClick), 'HTTPrequest("~w")', [HREF])
	},
	html(a([ %href(HREF),
		 onClick(OnClick),
		 onMouseOver('window.status=\'Edit file\'; return true;')
	       ],
	       img([ border=0,
		     height=H,
		     style('padding-top:4px'),
		     src='/edit.gif'
		 ]))).
edit_button(_, _) -->
	[].


%%	zoom_button(+Options)// is det.
%
%	Add zoom in/out button to show/hide the private documentation.

zoom_button(Base, Options) -->
	{   option(public_only(true), Options, true)
	->  format(string(HREF), '~w?public_only=false', [Base]),
	    Zoom = '/zoomin.gif'
	;   format(string(HREF), '~w?public_only=true', [Base]),
	    Zoom = '/zoomout.gif'
	},
	html(a(href=HREF,
	       img([ %class(icon),
		     border=0,
		     height=24,
		     style('padding-top:4px'),
		     src(Zoom)
		   ]))).
	

%%	objects(+Objects:list, +Mode, +Options)// is det.
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
	{ pi(Obj), !,
	  is_structured_comment(Comment, Prefixes),
	  indented_lines(Comment, Prefixes, Lines),
	  process_modes(Lines, Pos, Modes, Args, Lines1),
	  (   private(Obj, Options)
	  ->  Class = privdef		% private definition
	  ;   Class = pubdef		% public definition
	  ),
	  DOM = [\pred_dt(Modes, Class, Options), dd(class=defbody, DOM1)],
	  wiki_lines_to_dom(Lines1, Args, DOM0),
	  strip_leading_par(DOM0, DOM1)
	},
	need_mode(dl, Mode0, Mode),
	html(DOM).
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
	html_begin(Mode).

pop_mode(Mode, Stack, Stack) -->
	{ Stack = [Mode|_] }, !,
	[].
pop_mode(Mode, [H|Rest0], Rest) -->
	html_end(H),
	pop_mode(Mode, Rest0, Rest).

%%	undocumented(+Objects, +Options)// is det.
%
%	Describe undocumented predicates if the file is a module file.

undocumented(Objs, Options) -->
	{ memberchk(module(Module), Options),
	  memberchk(public(Exports), Options),
	  select_undocumented(Exports, Module, Objs, Undoc),
	  Undoc \== []
	}, !,
	html([ h2(class(undoc), 'Undocumented predicates'),
	       p(['The following predicates are exported, but not ',
		  'or incorrectly documented.'
		 ]),
	       dl(class(undoc),
		  \undocumented_predicates(Undoc, Options))
	     ]).
undocumented(_, _) -->
	[].

undocumented_predicates([], _) -->
	[].
undocumented_predicates([H|T], Options) -->
	undocumented_pred(H, Options),
	undocumented_predicates(T, Options).
		
undocumented_pred(Name/Arity, Options) -->
	{ functor(Head, Name, Arity) },
	html(dt(class=undoc, \pred_mode(Head, [], _, Options))).
		
select_undocumented([], _, _, []).
select_undocumented([PI|T0], M, Objs, [PI|T]) :-
	pi(PI),
	\+ in_doc(M:PI, Objs),
	select_undocumented(T0, M, Objs, T).
select_undocumented([_|T0], M, Objs, T) :-
	select_undocumented(T0, M, Objs, T).

in_doc(PI, Objs) :-
	member(doc(O,_,_), Objs),
	(   is_list(O)
	->  member(O2, O),
	    eq_pi(PI, O2)
	;   eq_pi(PI, O)
	).


%%	eq_pi(PI1, PI2) is semidet.
%
%	True if PI1 and PI2 refer to the same predicate.

eq_pi(PI, PI) :- !.
eq_pi(M:PI1, M:PI2) :-
	atom(M), !,
	eq_pi(PI1, PI2).
eq_pi(Name/A, Name//DCGA) :-
	A =:= DCGA+2, !.
eq_pi(Name//A, Name/DCGA) :-
	A =:= DCGA+2.

%%	pi(@Term) is semidet.
%
%	True if Term is a predicate indicator.

pi(_:PI) :- !,
	pi(PI).
pi(_/_).
pi(_//_).


		 /*******************************
		 *	       PRINT		*
		 *******************************/

%%	doc_write_html(+Out:stream, +Title:atomic, +DOM) is det.
%
%	Write HTML for the documentation page DOM using Title to Out.

doc_write_html(Out, Title, Doc) :-
	doc_page_dom(Title, Doc, DOM),
	phrase(html(DOM), Tokens),
	print_html_head(Out),
	print_html(Out, Tokens).

doc_page_dom(Title, Body, DOM) :-
	DOM = html([ head([ title(Title),
			    link([ rel(stylesheet),
				   type('text/css'),
				   href('pldoc.css')
				 ]),
			    script(src('/pldoc.js'), [])
			  ]),
		     body(Body)
		   ]).

print_html_head(Out) :-
	format(Out,
	       '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" \
	       "http://www.w3.org/TR/html4/strict.dtd">~n', []).

% Rendering rules
%
% These rules translate \-terms produced by wiki.pl

tags(Tags) -->
	html(dl(class=tags, Tags)).

tag(Tag, Value) -->
	{   tag_title(Tag, Title)
	->  true
	;   Title = Tag
	},
	{   tag_class(Tag, Class)
	->  true
	;   Class = tag
	},
	html([dt(class=Class, Title), dd(Value)]).

tag_title(compat, 'Compatibility:').
tag_title(tbd,    'To be done:').
tag_title(see,    'See also:').

tag_class(tbd, 		warn).
tag_class(bug, 		error).
tag_class(depreciated,	warn).

params(Params) -->
	html([ dt(class=tag, 'Parameters:'),
	       dd(table(class=paramlist,
			\param_list(Params)))
	     ]).

param_list([]) -->
	[].
param_list([H|T]) -->
	param(H),
	param_list(T).

param(param(Name,Descr)) -->
	html(tr([td(var(Name)), td(class=argdescr, ['- '|Descr])])).


		 /*******************************
		 *	      SECTIONS		*
		 *******************************/

section(Type, Title) -->
	{ wiki_string_to_dom(Title, [], Content0),
	  strip_leading_par(Content0, Content),
	  make_section(Type, Content, HTML)
	},
	html(HTML).

make_section(module,  Title, h1(class=module,  Title)).
make_section(section, Title, h1(class=section, Title)).


		 /*******************************
		 *	 PRED MODE HEADER	*
		 *******************************/

%%	pred_dt(+Modes)// is det.
%%	pred_dt(+Modes, +Class, Options)// is det.
%
%	Emit the predicate header.
%	
%	@param Modes	List as returned by process_modes/5.

pred_dt(Modes) -->
	pred_dt(Modes, preddef, []).

pred_dt(Modes, Class, Options) -->
	html(dt(class=Class,
		\pred_modes(Modes, [], _Done, Options))).

pred_modes([], Done, Done, _) -->
	[].
pred_modes([H|T], Done0, Done, Options) -->
	pred_mode(H, Done0, Done1, Options),
	pred_modes(T, Done1, Done, Options).
		
pred_mode(mode(Head,Vars), Done0, Done, Options) --> !,
	{ bind_vars(Vars) },
	pred_mode(Head, Done0, Done, Options).
pred_mode(Head is Det, Done0, Done, Options) --> !,
	anchored_pred_head(Head, Done0, Done, Options),
	pred_det(Det),
	html(div(style('clear:both'), [])).
pred_mode(Head, Done0, Done, Options) -->
	anchored_pred_head(Head, Done0, Done, Options),
	html(div(style('clear:both'), [])).

bind_vars([]).
bind_vars([Name=Var|T]) :-
	Var = '$VAR'(Name),
	bind_vars(T).

anchored_pred_head(Head, Done0, Done, Options) -->
	{ anchor_name(Head, PI, Name)
	},
	(   { memberchk(PI, Done0) }
	->  { Done = Done0 },
	    pred_head(Head)
	;   pred_edit_button(Head, Options),
	    html(a(name=Name, \pred_head(Head))),
	    { Done = [PI|Done0] }
	).

pred_edit_button(//(Head), Options) -->
	{ functor(Head, Name, Arity),
	  PredArity is Arity + 2
	},
	pred_edit_button(Name/PredArity, Options).
pred_edit_button(Name/Arity, Options) -->
	{ www_form_encode(Name, QName),
	  (   option(module(M), Options, []), M \== []
	  ->  www_form_encode(M, QM),
	      format(string(OnClick),
		     'HTTPrequest("/edit?name=~w&arity=~w&module=~w")',
		     [QName, Arity, QM])
	  ;   format(string(OnClick),
		     'HTTPrequest("/edit?name=~w&arity=~w")',
		     [QName, Arity])
	  )
	},
	html(div(style('float:right'),
		 a([ onClick(OnClick)
		   ],
		   img([ border=0,
			 height=16,
			 style('padding-top:2px'),
			 src='/edit.gif'
		       ])))).
pred_edit_button(Head, Options) -->
	{ functor(Head, Name, Arity)
	},
	pred_edit_button(Name/Arity, Options).


%%	pred_head(+Term) is det.
%
%	Emit a predicate head. The functor is  typeset as a =span= using
%	class =pred= and the arguments and =var= using class =arglist=.

pred_head(//(Head)) --> !,
	pred_head(Head),
	html(//).
pred_head(Head) -->
	{ atom(Head) }, !,
	html(b(class=pred, Head)).
pred_head(Head) -->			% Infix operators
	{ Head =.. [Functor,Left,Right],
	  current_op(_,Type,Functor),
	  op_type(Type, infix), !
	},
	html([ var(class=arglist, \pred_arg(Left, 1)),
	       span(class=pred, Functor),
	       var(class=arglist, \pred_arg(Right, 2))
	     ]).
pred_head(Head) -->			% Prefix operators
	{ Head =.. [Functor,Arg],
	  current_op(_,Type,Functor),
	  op_type(Type, prefix), !
	},
	html([ span(class=pred, Functor),
	       var(class=arglist, \pred_arg(Arg, 1))
	     ]).
pred_head(Head) -->			% Postfix operators
	{ Head =.. [Functor,Arg],
	  current_op(_,Type,Functor),
	  op_type(Type, postfix), !
	},
	html([ var(class=arglist, \pred_arg(Arg, 1)),
	       span(class=pred, Functor)
	     ]).
pred_head(Head) -->			% Plain terms
	{ Head =.. [Functor|Args] },
	html([ span(class=pred, Functor),
	       var(class=arglist,
		   [ '(', \pred_args(Args, 1), ')' ])
	     ]).

op_type(xf,  prefix).
op_type(yf,  prefix).
op_type(fx,  postfix).
op_type(fy,  postfix).
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
	;   html(', '),
	    { I2 is I + 1 },
	    pred_args(T, I2)
	).

pred_arg(Var, I) -->
	{ var(Var) }, !,
	html(['Arg', I]).
pred_arg(...(Term), I) --> !,
	pred_arg(Term, I),
	html('...').
pred_arg(Term, I) -->
	{ Term =.. [Ind,Arg],
	  mode_indicator(Ind)
	}, !,
	html([Ind, \pred_arg(Arg, I)]).
pred_arg(Arg:Type, _) --> !,
	html([\argname(Arg), :, \argtype(Type)]).
pred_arg(Arg, _) -->
	argname(Arg).

argname('$VAR'(Name)) --> !,
	html(Name).
argname(Name) --> !,
	html(Name).

argtype(Term) -->
	{ format(string(S), '~W',
		 [ Term,
		   [ quoted(true),
		     numbervars(true)
		   ]
		 ]) },
	html(S).

pred_det(unknown) -->
	[].
pred_det(Det) -->
	html([' is ', b(class=det, Det)]).


%%	term(+Term, +Bindings)// is det.
%
%	Process the \term element.
%	
%	@tbd	Properly merge with pred_head//1

term(Atom, []) -->
	{ atomic(Atom) }, !,
	html(span(class=functor, Atom)).
term(Term, Bindings) -->
	{ is_mode(Term is det),		% HACK. Bit too strict?
	  bind_vars(Bindings)
	}, !,
	pred_head(Term).
term(Term, Bindings) -->
	{ bind_vars(Bindings) },
	argtype(Term).
	

		 /*******************************
		 *	       PREDREF		*
		 *******************************/

%%	predref(+PI)// is det.
%
%	Create a reference to a predicate. The reference consists of the
%	relative path to the  file  using   the  predicate  indicator as
%	anchor.
%	
%	Current file must  be  available   through  the  global variable
%	=pldoc_file=.

predref(Name/Arity) -->
	{ functor(Term, Name, Arity),
	  predicate_property(system:Term, built_in), !,
	  format(string(FragmentId), '~w/~d', [Name, Arity]),
	  www_form_encode(FragmentId, EncId),
	  format(string(HREF), '/man?predicate=~w', [EncId])
	},
	html(a([class=builtin, href=HREF], [Name, /, Arity])).
predref(Name/Arity) -->
	{ pred_href(Name/Arity, HREF) }, !,
	html(a(href=HREF, [Name, /, Arity])).
predref(Name//Arity) -->
	{ PredArity is Arity + 2,
	  pred_href(Name/PredArity, HREF)
	}, !,
	html(a(href=HREF, [Name, //, Arity])).
predref(Name/Arity) -->
	html(span(class=undef, [Name, /, Arity])).
predref(Name//Arity) -->
	html(span(class=undef, [Name, //, Arity])).

pred_href(Name/Arity, HREF) :-
	format(string(FragmentId), '~w/~d', [Name, Arity]),
	www_form_encode(FragmentId, EncId),
	functor(Head, Name, Arity),
	(   catch(relative_file(Head, File), _, fail)
	->  format(string(HREF), '~w#~w', [File, EncId])
	;   in_file(Head, File)
	->  format(string(HREF), '/doc~w#~w', [File, EncId])
	;   HREF='#OOPS'		% TBD
	).

relative_file(Head, '') :-
	b_getval(pldoc_file, CurrentFile), CurrentFile \== [],
	in_file(Head, CurrentFile), !.
relative_file(Head, RelFile) :-
	b_getval(pldoc_file, CurrentFile), CurrentFile \== [],
	in_file(Head, DefFile),
	relative_file_name(DefFile, CurrentFile, RelFile).
	
%%	file(+FileName)// is det.
%
%	Create a link to another filename if the file exists.

file(File) -->
	{ b_getval(pldoc_file, CurrentFile),
	  absolute_file_name(File, _,
			     [ relative_to(CurrentFile),
			       access(read),
			       file_errors(fail)
			     ]), !
	},
	html(a([class(file), href(File)],File)).
file(File) -->
	html(code(class(file), File)).

%%	in_file(+Head, ?File) is nondet.
%
%	@tbd: prefer local, then imported, then `just anywhere'

in_file(Head, File) :-
	xref_current_source(File),
	atom(File),			% only plain files
	xref_defined(File, Head, How),
	How \= imported(_From).
in_file(Head, File) :-
	source_file(Head, File).
in_file(Head, File) :-
	current_module(Module),
	source_file(Module:Head, File).


%%	relative_file_name(+Path:atom, +RelTo:atom, -RelPath:atom) is det.
%
%	Create a relative path from an absolute one.
%	
%	@tbd	move to library?

relative_file_name(Path, RelTo, RelPath) :-
        concat_atom(PL, /, Path),
        concat_atom(RL, /, RelTo),
        delete_common_prefix(PL, RL, PL1, PL2),
        to_dot_dot(PL2, DotDot, PL1),
        concat_atom(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
        delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
        to_dot_dot(T0, T, Tail).



		 /*******************************
		 *	      ANCHORS		*
		 *******************************/

%%	anchor_name(+Head, -PI:atom/integer, -Anchor:string) is det.
%
%	Create an HTML anchor name from Head.

anchor_name(//(Head), Name/Arity, Anchor) :- !,
	functor(Head, Name, DCGArity),
	Arity is DCGArity+2,
	format(string(Anchor), '~w/~d', [Name, Arity]).
anchor_name(Head, Name/Arity, Anchor) :-
	functor(Head, Name, Arity),
	format(string(Anchor), '~w/~d', [Name, Arity]).
