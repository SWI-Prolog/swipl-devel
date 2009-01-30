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
	    index_man_file/2,		% +Class, +FileSpec
					% HTML generation
	    man_page//2,		% +Obj, +Options
	    man_overview//1		% +Options
	  ]).
:- use_module(library(sgml)).
:- use_module(library(occurs)).
:- use_module(library(lists)).
:- use_module(library(url)).
:- use_module(library(apply)).
:- use_module(doc_wiki).
:- use_module(doc_html).
:- use_module(doc_search).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(doc_http)).
:- include(hooks).

/** <module> Process SWI-Prolog HTML manuals

*/

:- dynamic
	man_index/5.		% Object, Summary, File, Class, Offset

%%	clean_man_index is det.
%
%	Clean already loaded manual index.

clean_man_index :-
	retractall(man_index(_,_,_,_,_)).


%%	manual_directory(-Class, -Dir)// is nondet.
%
%	True if Dir is a directory holding manual files. Class is an
%	identifier used by doc_object_summary/4.


manual_directory(manual,   swi('doc/Manual')).
manual_directory(packages, swi('doc/packages')).



		 /*******************************
		 *	    PARSE MANUAL	*
		 *******************************/

%%	index_manual is det.
%
%	Load the manual index if not already done.

index_manual :-
	man_index(_,_,_,_,_), !.
index_manual :-
	(   manual_directory(Class, Dir),
	    index_man_directory(Dir,
				[ class(Class),
				  file_errors(fail)
				]),
	    fail ; true
	).


%%	index_man_directory(Dir, +Options) is det
%
%	Index  the  HTML  directory   Dir.    Options are:
%	
%		* class(Class)
%		Define category of the found objects.
%		
%	Remaining Options are passed to absolute_file_name/3.

index_man_directory(Spec, Options) :-
	(   select(class(Class), Options, Options1)
	->  true
	;   Options1 = Options,
	    Class = misc
	),
	absolute_file_name(Spec, Dir,
			   [ file_type(directory),
			     access(read)
			   | Options1
			   ]),
	atom_concat(Dir, '/*.html', Pattern),
	expand_file_name(Pattern, Files),
	maplist(index_man_file(Class), Files).


%%	index_man_file(+Class, +File)
%
%	Collect the documented objects from the SWI-Prolog manual file
%	File.

index_man_file(Class, File) :-
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
	nb_setval(pldoc_index_class, Class),
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
%	Called from sgml_parse/2 in  index_man_file/2.   Element  is the
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
        memberchk(name=Id, AA),
	atom_to_pi(Id, PI),
	nb_setval(pldoc_man_index, dd(PI, File, Offset)).
index_on_begin(dd, _, Parser) :- !,
	nb_getval(pldoc_man_index, dd(Name/Arity, File, Offset)),
	nb_setval(pldoc_man_index, []),
	sgml_parse(Parser,
		   [ document(DD),
		     parse(content)
		   ]),
	summary(DD, Summary),
	nb_getval(pldoc_index_class, Class),
        assert(man_index(Name/Arity, Summary, File, Class, Offset)).
index_on_begin(div, Attributes, Parser) :- !,
	memberchk(class=title, Attributes),
	get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(DOM),
		     parse(content)
		   ]),
	dom_to_text(DOM, Title),
	nb_getval(pldoc_index_class, Class),
	assert(man_index(section(0, '0', File), Title, File, Class, Offset)).
index_on_begin(H, _, Parser) :-		% TBD: add class for document title.
	heading(H, Level),
	get_sgml_parser(Parser, charpos(Offset)),
        get_sgml_parser(Parser, file(File)),
	sgml_parse(Parser,
		   [ document(Doc),
		     parse(content)
		   ]),
	dom_section(Doc, Nr, Title),
	nb_getval(pldoc_index_class, Class),
	assert(man_index(section(Level, Nr, File), Title, File, Class, Offset)).
	
%%	dom_section(+HeaderDOM, -NR, -Title) is semidet.
%
%	NR is the section number (e.g. 1.1, 1.23) and Title is the title
%	from a section header. The  first   clauses  processes the style
%	information from latex2html, emitting sections as:
%	
%	==
%	<HN> <A name="sec:nr"><span class='sec-nr'>NR</span>|_|
%			      <span class='sec-title'>Title</span>
%	==

dom_section(DOM, Nr, Title) :-
	sub_term([ element(span, A1, [Nr]) | Rest ], DOM),
	append(_Sep, [element(span, A2, TitleDOM)], Rest),
	memberchk(class='sec-nr', A1),
	memberchk(class='sec-title', A2), !,
	dom_to_text(TitleDOM, Title).
dom_section(DOM, Nr, Title) :-
	dom_to_text(DOM, Title),
	section_number(Title, Nr, Title).

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

load_man_object(Obj, ParentSection, Path, DOM) :-
	resolve_section(Obj, For),
	For = section(_,SN,Path),
	parent_section(For, ParentSection),
	findall(Nr-Pos, section_start(Path, Nr, Pos), Pairs),
	(   (   Pairs = [SN-_|_]
	    ;	Pairs == []
	    )
	->  !,
	    load_html_file(Path, DOM)		% Load whole file
	;   append(_, [SN-Start|Rest], Pairs)
	->  !,
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
			 source(In),
			 syntax_errors(quiet)
		       | Options
		       ]),
	    free_sgml_parser(Parser),
	    close(In)
	).
load_man_object(For, Parent, Path, DOM) :-
	index_manual,
	object_spec(For, Obj),
	man_index(Obj, _, Path, _, Position),
	(   object_section(Path, Position, Parent)
	->  true
	;   Parent = Path
	),
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
	man_index(section(_,Nr,_), _, Path, _, Pos).

%%	resolve_section(+SecIn, -SecOut) is det.
%
%	Resolve symbolic path reference and fill   in  level and section
%	number if this information is missing.   The latter allows us to
%	refer to files of the manual.

resolve_section(section(Level, No, Spec),
		section(Level, No, Path)) :-
	absolute_file_name(Spec, Path,
			   [ access(read)
			   ]),
	(   (   var(Level)
	    ;   var(Path)
	    )
	->  index_manual,
	    ignore(man_index(section(Level, No, Path), _, _, _, _))
	;   true
	).

%%	parent_section(+Section, +Parent) is det.
%
%	Parent is the parent-section  of   Section.  First  computes the
%	section number and than finds the   required  number in the same
%	file or same directory.

parent_section(section(Level, Nr, File), Parent) :-
	integer(Level),
	Parent = section(PL, PNr, _PFile),
	PL is Level - 1,
	findall(B, sub_atom(Nr, B, _, _, '.'), BL),
	last(BL, Before),
	sub_atom(Nr, 0, Before, _, PNr),
	(   man_index(Parent, _, File, _, _)
	->  true
	;   man_index(Parent, _, ParentFile, _, _),
	    same_dir(File, ParentFile)
	->  true
	;   man_index(Parent, _, _, _, _)
	), !.
parent_section(section(_, _, File), File).

%%	object_section(+Path, +Position, -Section) is semidet.
%
%	Section is the section in which object appears.  This is the
%	last section object before position.

object_section(Path, Pos, Section) :-
	Section	= section(_,_,_),
	findall(Section,
	       (man_index(Section, _, Path, _, SecPos), SecPos =< Pos),
		List),
	last(List, Section).

same_dir(File1, File2) :-
	file_directory_name(File1, Dir),
	file_directory_name(File2, Dir).

%%	object_spec(+Atom, -SpecTerm)
%
%	Tranform the Name/Arity, etc strings as   received from the HTTP
%	into a term.  Must return unique results.

object_spec(Spec, Spec).
object_spec(Atom, Spec) :-
	catch(atom_to_term(Atom, Spec, _), _, fail), !,
	Atom \== Spec.
object_spec(Atom, PI) :-
	atom_to_pi(Atom, PI).


		 /*******************************
		 *	      EMIT    		*
		 *******************************/

%%	man_page(+Obj, +Options)// is semidet.
%
%	Produce a Prolog manual page for  Obj.   The  page consists of a
%	link to the section-file and  a   search  field, followed by the
%	predicate description.  Obj is one of:
%	
%	    * Name/Arity
%	    Predicate indicator: display documentation of the predicate

%	    * section(Level, Number, File)
%	    Display a section of the manual
%	
%	Options:
%	
%		* no_manual(Action)
%		If Action = =fail=, fail instead of displaying a
%		not-found message.
%		
%		* links(Bool)
%		If =true= (default), include links to the parent object;
%		if =false=, just emit the manual material.

man_page(Obj, Options) -->
	{ findall((Parent+Path)-DOM,
		  load_man_object(Obj, Parent, Path, DOM),
		  Matches),
	  Matches = [Parent+Path-_|_]
	},
	html_requires(pldoc),
	(   { option(links(true), Options, true) }
	->  man_links(Parent, Options),
	    html(p([]))
	;   []
	),
	man_matches(Matches).
man_page(Obj, Options) -->
	{ \+ option(no_manual(fail), Options),
	  term_to_atom(Obj, Atom)
	},
	html_requires(pldoc),
	(   { option(links(true), Options, true) }
	->  man_links([], [])
	;   html(p(['No manual entry for ', Atom]))
	).

man_matches([]) -->
	[].
man_matches([(_Parent+Path)-H|T]) -->
	dom_list(H, Path),
	man_matches(T).

dom_list([], _) -->
	[].
dom_list([H|T], Path) -->
	dom(H, Path),
	dom_list(T, Path).

dom(element(E, Atts, Content), Path) --> !,
	dom_element(E, Atts, Content, Path).
dom(CDATA, _) -->
	html(CDATA).

dom_element(a, _, [], _) -->			% Useless back-references
	[].
dom_element(a, Att, Content, Path) -->
	{ memberchk(href=HREF, Att),
	  (   memberchk(class=Class, Att)
	  ->  true
	  ;   Class = unknown
	  ),
	  rewrite_ref(Class, HREF, Path, Myref)
	}, !,
	html(a(href(Myref), \dom_list(Content, Path))).
dom_element(div, Att, _, _) -->
	{ memberchk(class=navigate, Att) }, !.
dom_element(html, _, Content, Path) --> !,	% do not emit a html for the second time
	dom_list(Content, Path).
dom_element(head, _, Content, Path) --> !,	% do not emit a head for the second time
	dom_list(Content, Path).
dom_element(title, _, _, _) --> !.
dom_element(link, _, _, _) --> !.
dom_element(body, _, Content, Path) --> !,	% do not emit a body for the second time
	dom_list(Content, Path).
dom_element(Name, Attrs, Content, Path) -->
	{ Begin =.. [Name|Attrs] },
	html_begin(Begin),
	dom_list(Content, Path),
	html_end(Name).


%%	rewrite_ref(+Class, +Ref0, +Path, -ManRef) is semidet.
%
%	Rewrite Ref0 from the HTML reference manual format to the server
%	format. Reformatted:
%	
%		$ File#Name/Arity :
%		Local reference using the manual presentation
%		=|/man?predicate=PI|=.
%		
%		$ File#sec:NR :
%		Rewrite to =|section(Level, NT, FilePath)|=
%		
%		$ File#flag:Name :
%		Rewrite to =|section(Level, NT, FilePath)#flag:Name|=
%		
%	@param Class	Class of the <A>.  Supported classes are
%	
%		| sec  | Link to a section     |
%		| pred | Link to a predicate   |
%		| flag | link to a Prolog flag |
%		
%	@param Ref0	Initial reference from the =a= element
%	@param Path	Currently loaded file
%	@param ManRef	PlDoc server reference

rewrite_ref(pred, Ref0, _, Ref) :-		% Predicate reference
	sub_atom(Ref0, _, _, A, '#'), !,
	sub_atom(Ref0, _, A, 0, Fragment),
	atom_to_pi(Fragment, PI),
	man_index(PI, _, _, _, _),
	www_form_encode(Fragment, Enc),
	http_location_by_id(pldoc_man, ManHandler),
	format(string(Ref), '~w?predicate=~w', [ManHandler, Enc]).
rewrite_ref(sec, Ref0, Path, Ref) :-		% Section inside a file
	sub_atom(Ref0, B, _, A, '#'), !,
	sub_atom(Ref0, _, A, 0, Fragment),
	sub_atom(Ref0, 0, B, _, File),
	referenced_section(Fragment, File, Path, Section),
	object_href(Section, Ref).
rewrite_ref(sec, File, Path, Ref) :-		% Section is a file
	file_directory_name(Path, Dir),
	concat_atom([Dir, /, File], SecPath),
	Obj = section(_, _, SecPath),
	man_index(Obj, _, _, _, _), !,
	object_href(Obj, Ref).
rewrite_ref(flag, Ref0, Path, Ref) :-
	sub_atom(Ref0, B, _, A, '#'), !,
	sub_atom(Ref0, 0, B, _, File),
	sub_atom(Ref0, _, A, 0, Fragment),
	file_directory_name(Path, Dir),
	concat_atom([Dir, /, File], SecPath),
	Obj = section(_, _, SecPath),
	man_index(Obj, _, _, _, _), !,
	object_href(Obj, Ref1),
	format(string(Ref), '~w#~w', [Ref1, Fragment]).

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
	(   File == ''
	->  SecPath = Path
	;   file_directory_name(Path, Dir),
	    concat_atom([Dir, /, File], SecPath)
	),
	man_index(section(Level, Nr, SecPath), _, _, _, _).


%%	man_links(+Parent, +Options)// is det.
%
%	Create top link structure for manual pages.

man_links(Parent, Options) -->
	html(div(class(navhdr),
		 [ div(class(jump), \man_parent(Parent)),
		   div(class(search), \search_form(Options)),
		   br(clear(right))
		 ])).

man_parent(Section) -->
	{ Section = section(_,_,_)
	}, !,
	object_ref(Section, [secref_style(number_title)]).
man_parent(File) -->
	{ atom(File),
	  Obj = section(_,_,_),
	  man_index(Obj, _Title, File, _Class, _Offset)
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
	{ man_index(Obj, Title, _File, _Class, _Offset)
	},
	html(Title).
section_link(_, Obj, _Options) --> !,
	{ Obj = section(_, Number, _),
	  man_index(Obj, Title, _File, _Class, _Offset)
	},
	(   { Number == '0' }
	->  html(Title)
	;   html([Number, ' ', Title])
	).


		 /*******************************
		 *	 INDICES & OVERVIEW	*
		 *******************************/

%%	man_overview(+Options)// is det.
%
%	Provide a toplevel overview on the  manual: the reference manual
%	and the available packages.

man_overview(Options) -->
	{ http_absolute_location(pldoc_man(.), RefMan, [])
	},
	html([ blockquote(class(refman_link),
			  a(href(RefMan),
			    'SWI-Prolog reference manual')),
	       h2(class(package_doc_title),
		  'SWI-Prolog package documentation'),
	       blockquote(class(package_overview),
			  \packages(Options))
	     ]).

packages(Options) -->
	{ findall(Pkg, current_package(Pkg), Pkgs)
	},
	packages(Pkgs, Options).

packages([], _) -->
	[].
packages([Pkg|T], Options) -->
	package(Pkg, Options),
	packages(T, Options).

package(pkg(Title, HREF, HavePackage), Options) -->
	{ package_class(HavePackage, Class, Options)
	},
	html(div(class(Class),
		 a([href(HREF)], Title))).

package_class(true,  pkg_link, _).
package_class(false, no_pkg_link, _).

current_package(pkg(Title, HREF, HavePackage)) :-
	man_index(section(0, _, _), Title, File, packages, _),
	file_base_name(File, FileNoDir),
	file_name_extension(Base, _, FileNoDir),
	(   exists_source(library(Base))
	->  HavePackage = true
	;   HavePackage = false
	),
	http_absolute_location(pldoc_pkg(FileNoDir), HREF, []).


:- http_handler(pldoc_pkg(.), pldoc_package, [prefix]).

%%	pldoc_package(+Request)
%
%	HTTP handler for PlDoc package documentation.  Accepts
%	/pkg/<package>.html.

pldoc_package(Request) :-
	(   memberchk(path_info(PkgDoc), Request),
	    \+ sub_atom(PkgDoc, _, _, _, /),
	    Obj = section(0,_,_),
	    index_manual,
	    man_index(Obj, Title, File, packages, _),
	    file_base_name(File, PkgDoc)
	->  reply_html_page(title(Title),
			    \object_page(Obj, []))
	;   memberchk(path(Path), Request),
	    existence_error(http_location, Path)
	).
	

		 /*******************************
		 *	    HOOK SEARCH		*
		 *******************************/

prolog:doc_object_summary(Obj, Class, File, Summary) :-
	index_manual,
	man_index(Obj, Summary, File, Class, _Offset).
	
prolog:doc_object_page(Obj, Options) -->
	man_page(Obj, [no_manual(fail)|Options]).

prolog:doc_object_link(Obj, Options) -->
	{ Obj = section(_,_,_) }, !,
	section_link(Obj, Options).

prolog:doc_category(manual,   30, 'SWI-Prolog Reference Manual').
prolog:doc_category(packages, 40, 'Package documentation').

prolog:doc_file_index_header(File, Options) -->
	{ Section = section(_Level, _No, File),
	  man_index(Section, _Summary, File, _Cat, _Offset)
	}, !,
	html(tr(th([colspan(3), class(section)],
		   [ \object_ref(Section,
				 [ secref_style(number_title)
				 | Options
				 ])
		   ]))).

prolog:doc_object_title(Obj, Title) :-
	Obj = section(_,_,_),
	man_index(Obj, Title, _, _, _), !.

prolog:doc_canonical_object(section(Level, No, Path),
			    section(Level, No, swi(Local))) :-
	is_absolute_file_name(Path),
	absolute_file_name(swi(.), SWI,
			   [ file_type(directory),
			     solutions(all)
			   ]),
	atom_concat(SWI, Local, Path), !.
