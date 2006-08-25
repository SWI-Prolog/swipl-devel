/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2006, University of Amsterdam

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

:- module(pldoc_man,
	  [ clean_man_index/0,		% 
	    index_man_directory/2,	% +DirSpec, +Options
	    index_man_file/1,		% +FileSpec
	    man_page/4			% +Obj, +Options, //
	  ]).
:- use_module(library(sgml)).
:- use_module(library(occurs)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(doc_wiki).
:- use_module(doc_html).
:- use_module(doc_search).
:- use_module(library('http/html_write')).
:- include(hooks).

/** <module> Process SWI-Prolog HTML manuals

*/

:- dynamic
	man_index/4.			% Object, Summary, File, Offset

%%	clean_man_index is det.
%
%	Clean already loaded manual index.

clean_man_index :-
	retractall(man_index(_,_,_,_)).


%%	manual_directory(-Dir)// is nondet.
%
%	True if Dir is a directory holding manual files.


manual_directory(swi('doc/Manual')).
manual_directory(swi('doc/packages')).



		 /*******************************
		 *	    PARSE MANUAL	*
		 *******************************/

%%	index_manual is det.
%
%	Load the manual index if not already done.

index_manual :-
	man_index(_,_,_,_), !.
index_manual :-
	(   manual_directory(Dir),
	    index_man_directory(Dir, [file_errors(fail)]),
	    fail ; true
	).


%%	index_man_directory(Dir, +Options) is det
%
%	Index  the  HTML  directory   Dir.    Options   are   passed  to
%	absolute_file_name/3.

index_man_directory(Spec, Options) :-
	absolute_file_name(Spec, Dir,
			   [ file_type(directory),
			     access(read)
			   | Options
			   ]),
	atom_concat(Dir, '/*.html', Pattern),
	expand_file_name(Pattern, Files),
	maplist(index_man_file, Files).


%%	index_man_file(+File)
%
%	Collect the documented objects from the SWI-Prolog manual file
%	File.

index_man_file(File) :-
	absolute_file_name(File, Path,
			   [ access(read)
			   ]),
	open(Path, read, In, [type(binary)]),
	dtd(html, DTD),
        new_sgml_parser(Parser, [dtd(DTD)]),
        set_sgml_parser(Parser, file(File)),
        set_sgml_parser(Parser, dialect(sgml)),
	set_sgml_parser(Parser, shorttag(false)),
	nb_setval(pldoc_man_index, []),
	call_cleanup(sgml_parse(Parser,
				[ source(In),
				  call(begin, index_on_begin)
				]),
		     (	 free_sgml_parser(Parser),
			 close(In),
			 nb_delete(pldoc_man_index)
		     )).


%%	index_on_begin(+Element, +Attributes, +Parser) is semidet.
%
%	Called from sgml_parse/2 in  index_man_file/1.   Element  is the
%	name of the element, Attributes the  list of Name=Value pairs of
%	the open attributes. Parser is the parser objects.

index_on_begin(dt, Attributes, Parser) :- !,
	memberchk(class=pubdef, Attributes),
        get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(DT),
		     parse(content)
		   ]),
	sub_term(element(a, AA, _), DT),
        memberchk(name=Id, AA), !,
	concat_atom([Name, ArityAtom], /, Id),
	catch(atom_number(ArityAtom, Arity), _, fail),
	integer(Arity),
	Arity > 0,
	nb_setval(pldoc_man_index, dd(Name/Arity, File, Offset)).
index_on_begin(dd, _, Parser) :- !,
	nb_getval(pldoc_man_index, dd(Name/Arity, File, Offset)),
	nb_setval(pldoc_man_index, []),
	sgml_parse(Parser,
		   [ document(DD),
		     parse(content)
		   ]),
	summary(DD, Summary),
        assert(man_index(Name/Arity, Summary, File, Offset)).
index_on_begin(div, Attributes, Parser) :- !,
	memberchk(class=title, Attributes),
	get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(DOM),
		     parse(content)
		   ]),
	dom_to_text(DOM, Title),
	assert(man_index(section(0, '0', File), Title, File, Offset)).
index_on_begin(H, _, Parser) :-		% TBD: add class for document title.
	heading(H, Level),
	get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(Doc),
		     parse(content)
		   ]),
	dom_to_text(Doc, Title),
	section_number(Title, Nr, PlainTitle),
	assert(man_index(section(Level, Nr, File), PlainTitle, File, Offset)).
	
section_number(Title, Nr, PlainTitle) :-
	sub_atom(Title, 0, 1, _, Start),
	(   char_type(Start, digit)
	->  true
	;   char_type(Start, upper),
	    sub_atom(Title, 1, 1, _, '.') 	% A., etc: Appendices
	),
	sub_atom(Title, B, _, A, ' '), !,
	sub_atom(Title, 0, B, _, Nr),
	sub_string(Title, _, A, 0, PlainTitle).

heading(h1, 1).
heading(h2, 2).
heading(h3, 3).
heading(h4, 4).


%%	summary(+DOM, -Summary:string) is det.
%
%	Summary is the first sentence of DOM.

summary(DOM, Summary) :-
	phrase(summary(DOM, _), SummaryCodes0),
	phrase(normalise_white_space(SummaryCodes), SummaryCodes0),
	string_to_list(Summary, SummaryCodes).

summary([], _) --> !,
	[].
summary(_, Done) -->
	{ Done == true }, !,
	[].
summary([element(_,_,Content)|T], Done) --> !,
	summary(Content, Done),
	summary(T, Done).
summary([CDATA|T], Done) -->
	{ atom_codes(CDATA, Codes)
	},
	(   { Codes = [Period|Rest],
	      code_type(Period, period),
	      space(Rest)
	    }
	->  [ Period ],
	    { Done = true }
	;   { append(Sentence, [C, Period|Rest], Codes),
	      code_type(Period, period),
	      \+ code_type(C, period),
	      space(Rest)
	    }
	->  string(Sentence),
	    [C, Period],
	    { Done = true }
	;   string(Codes),
	    summary(T, Done)
	).
	
string([]) -->
	[].
string([H|T]) -->
	[H],
	string(T).

space([C|_]) :- code_type(C, space), !.
space([]).

%%	dom_to_text(+DOM, -Text)
%
%	Extract the text of a parsed HTML term.  White-space in the
%	result is normalised.  See normalise_white_space//1.

dom_to_text(Dom, Text) :-
	phrase(cdata_list(Dom), CDATA),
	with_output_to(codes(Codes0),
		       forall(member(T, CDATA),
			      write(T))),
	phrase(normalise_white_space(Codes), Codes0),
	string_to_list(Text, Codes).

cdata_list([]) -->
	[].
cdata_list([H|T]) -->
	cdata(H),
	cdata_list(T).

cdata(element(_, _, Content)) --> !,
	cdata_list(Content).
cdata(CDATA) -->
	{ atom(CDATA) }, !,
	[CDATA].
cdata(_) -->
	[].


		 /*******************************
		 *	      RETRIEVE		*
		 *******************************/

%%	load_man_object(+Obj, -Parent, -Path, -DOM) is nondet.
%
%	load the desription of the  object   matching  Obj from the HTML
%	sources and return the DT/DD pair in DOM.
%	
%	@tbd	Nondet?

load_man_object(For, ParentSection, Path, DOM) :-
	For = section(_,SN,Path),
	parent_section(For, ParentSection),
	findall(Nr-Pos, section_start(Path, Nr, Pos), Pairs),
	(   Pairs = [SN-_|_]
	->  load_html_file(Path, DOM)		% Load whole file
	;   append(_, [SN-Start|Rest], Pairs),
	    (	member(N-End, Rest),
		\+ sub_atom(N, 0, _, _, SN),
		Len is End - Start,
		Options = [content_length(Len)]
	    ->	true
	    ;	Options = []
	    ),
	    open(Path, read, In, [type(binary)]),
	    seek(In, Start, bof, _),
	    dtd(html, DTD),
	    new_sgml_parser(Parser,
			    [ dtd(DTD)
			    ]),
	    set_sgml_parser(Parser, file(Path)),
	    set_sgml_parser(Parser, dialect(sgml)),
	    set_sgml_parser(Parser, shorttag(false)),
	    set_sgml_parser(Parser, defaults(false)),
	    sgml_parse(Parser,
		       [ document(DOM),
			 source(In)
		       | Options
		       ]),
	    free_sgml_parser(Parser),
	    close(In)
	).
load_man_object(For, Path, Path, DOM) :-
	index_manual,
	object_spec(For, Obj),
	man_index(Obj, _, Path, Position),
	open(Path, read, In, [type(binary)]),
	seek(In, Position, bof, _),
	dtd(html, DTD),
	new_sgml_parser(Parser,
			[ dtd(DTD)
			]),
	set_sgml_parser(Parser, file(Path)),
        set_sgml_parser(Parser, dialect(sgml)),
	set_sgml_parser(Parser, shorttag(false)),
	set_sgml_parser(Parser, defaults(false)),
	sgml_parse(Parser,
		   [ document(DT),
		     source(In),
		     parse(element)
		   ]),
	sgml_parse(Parser,
		   [ document(DD),
		     source(In),
		     parse(element)
		   ]),
	free_sgml_parser(Parser),
	close(In),
	append(DT, DD, DOM).

section_start(Path, Nr, Pos) :-
	index_manual,
	man_index(section(_,Nr,_), _, Path, Pos).

%%	parent_section(+Section, +Parent) is det.
%
%	@tbd	How to avoid getting supper-section of another document?
%	
%			* Sort by same-file
%			* Sort by same/closest dir

parent_section(section(Level, Nr, _File), Parent) :-
	Parent = section(PL, PNr, _PFile),
	PL is Level - 1,
	findall(B, sub_atom(Nr, B, _, _, '.'), BL),
	last(BL, Before),
	sub_atom(Nr, 0, Before, _, PNr),
	man_index(Parent, _, _, _), !.
parent_section(section(_, _, File), File).

object_spec(Spec, Spec).
object_spec(Atom, PI) :-
	atom_to_pi(Atom, PI).


		 /*******************************
		 *	      EMIT    		*
		 *******************************/

%%	man_page(+Obj, +Options)// is semidet.
%
%	Produce a Prolog manual page for  Obj.   The  page consists of a
%	link to the section-file and  a   search  field, followed by the
%	predicate description.  Options:
%	
%		* no_manual(Action)
%		If Action = =fail=, fail instead of displaying a
%		not-found message.

man_page(Obj, _Options) -->
	{ load_man_object(Obj, Parent, Path, DOM), !
	},
	html([ \man_links(Parent, []),
	       p([]),
	       \dom_list(DOM, Path)
	     ]).
man_page(Obj, Options) -->
	{ \+ option(no_manual(fail), Options),
	  term_to_atom(Obj, Atom)
	},
	html([ \man_links([], []),	% Use index file?
	       'No manual entry for ', Atom
	     ]).

dom_list([], _) -->
	[].
dom_list([H|T], Path) -->
	dom(H, Path),
	dom_list(T, Path).

dom(element(a, _, []), _) -->		% Useless back-references
	[].
dom(element(a, Att, Content), Path) -->
	{ memberchk(href=HREF, Att),
	  rewrite_ref(HREF, Path, Myref)
	}, !,
	html(a(href(Myref), \dom_list(Content, Path))).
dom(element(div, Att, _), _) -->
	{ memberchk(class=navigate, Att) }, !.
dom(element(Name, Attrs, Content), Path) --> !,
	{ Begin =.. [Name|Attrs] },
	html_begin(Begin),
	dom_list(Content, Path),
	html_end(Name).
dom(CDATA, _) -->
	html(CDATA).


%%	rewrite_ref(+Ref0, +Path, -ManRef) is semidet.
%
%	Rewrite HREFS from the internal manual format to the server
%	format.  Reformatted:
%	
%		$ File#Name/Arity :
%		Local reference using the manual presentation
%		/man?predicate=PI.
%		
%		$ File#sec:NR :
%		Rewrite to section(Level, NT, FilePath)

rewrite_ref(Ref0, Path, Ref) :-
	sub_atom(Ref0, B, _, A, '#'), !,
	sub_atom(Ref0, _, A, 0, Fragment),
	(   atom_to_pi(Fragment, PI),
	    man_index(PI, _, _, _)
	->  www_form_encode(Fragment, Enc),
	    format(string(Ref), '/man?predicate=~w', [Enc])
	;   sub_atom(Ref0, 0, B, _, File),
	    referenced_section(Fragment, File, Path, Section),
	    object_href(Section, Ref)
	).


%%	atom_to_pi(+Atom, -PredicateIndicator) is semidet.
%	
%	If Atom is `Name/Arity', decompose to Name and Arity. No errors.

atom_to_pi(Atom, Name/Arity) :-
	atom(Atom),
	concat_atom([Name, AA], /, Atom),
	catch(atom_number(AA, Arity), _, fail),
	integer(Arity),
	Arity >= 0.

%%	referenced_section(+Fragment, +File, +Path, -Section)

referenced_section(Fragment, File, Path, section(Level, Nr, SecPath)) :-
	atom_concat('sec:', Nr, Fragment),
	findall(x, sub_atom(Nr, _, _, _, '.'), L),
	length(L, Nx),
	Level is Nx + 1,
	file_directory_name(Path, Dir),
	concat_atom([Dir, /, File], SecPath),
	man_index(section(Level, Nr, SecPath), _, _, _).


%%	man_links(+Parent, +Options)// is det.
%
%	Create top link structure for manual pages.

man_links(Parent, _Options) -->
	html(div(class(navhdr),
		 [ span(style('float:left'), \man_parent(Parent)),
		   span(style('float:right'), \search_form)
		 ])).

man_parent(Section) -->
	{ Section = section(_,_,_)
	}, !,
	object_ref(Section, [secref_style(number_title)]).
man_parent(File) -->
	{ atom(File),
	  Obj = section(_,_,_),
	  man_index(Obj, _Title, File, _Offset)
	}, !,
	object_ref(Obj, [secref_style(number_title)]).
man_parent(_) -->
	[].

%%	section_link(+Obj, +Options)// is det.
%
%	Create link to a section.  Options recognised:
%	
%		* secref_style(+Style)
%		One of =number=, =title= or =number_title=.

section_link(Section, Options) -->
	{ option(secref_style(Style), Options, number)
	},
	section_link(Style, Section, Options).

section_link(number, section(_, Number, _), _Options) --> !,
	(   {Number == '0'}		% Title.  Package?
	->  []
	;   html(['Sec. ', Number])
	).
section_link(title, Obj, _Options) --> !,
	{ man_index(Obj, Title, _File, _Offset)
	},
	html(Title).
section_link(_, Obj, _Options) --> !,
	{ Obj = section(_, Number, _),
	  man_index(Obj, Title, _File, _Offset)
	},
	(   { Number == '0' }
	->  html(Title)
	;   html([Number, ' ', Title])
	).


		 /*******************************
		 *	    HOOK SEARCH		*
		 *******************************/

prolog:doc_object_summary(Obj, manual, File, Summary) :-
	index_manual,
	man_index(Obj, Summary, File, _Offset).
	
prolog:doc_object_page(Obj, Options) -->
	man_page(Obj, [no_manual(fail)|Options]).

prolog:doc_object_link(Obj, Options) -->
	{ Obj = section(_,_,_) }, !,
	section_link(Obj, Options).
