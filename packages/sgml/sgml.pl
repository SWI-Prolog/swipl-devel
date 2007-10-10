/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(sgml,
	  [ load_sgml_file/2,		% +File, -ListOfContent
	    load_xml_file/2,		% +File, -ListOfContent
	    load_html_file/2,		% +File, -Document

	    load_structure/3,		% +File, -Term, +Options

	    load_dtd/2,			% +DTD, +File
	    load_dtd/3,			% +DTD, +File, +Options
	    dtd/2,			% +Type, -DTD
	    dtd_property/2,		% +DTD, ?Property

	    new_dtd/2,			% +Doctype, -DTD
	    free_dtd/1,			% +DTD
	    open_dtd/3,			% +DTD, +Options, -Stream

	    new_sgml_parser/2,		% -Parser, +Options
	    free_sgml_parser/1,		% +Parser
	    set_sgml_parser/2,		% +Parser, +Options
	    get_sgml_parser/2,		% +Parser, +Options
	    sgml_parse/2,		% +Parser, +Options

	    sgml_register_catalog_file/2, % +File, +StartOrEnd

	    xml_quote_attribute/3,	% +In, -Quoted, +Encoding
	    xml_quote_cdata/3,		% +In, -Quoted, +Encoding
	    xml_quote_attribute/2,	% +In, -Quoted
	    xml_quote_cdata/2,		% +In, -Quoted
	    xml_name/1,			% +In
	    xml_is_dom/1		% +Term
	  ]).
:- use_module(library(lists)).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(dtd, '.').
user:file_search_path(dtd, swi('library/DTD')).

sgml_register_catalog_file(File, Location) :-
	prolog_to_os_filename(File, OsFile),
	'_sgml_register_catalog_file'(OsFile, Location).

load_foreign :-
	current_predicate(_, _:sgml_parse(_,_)), !.
load_foreign :-
	load_foreign_library(foreign(sgml2pl)).

register_catalog(Base) :-
	absolute_file_name(dtd(Base),
			       [ extensions([soc]),
				 access(read),
				 file_errors(fail)
			       ],
			       SocFile),
	sgml_register_catalog_file(SocFile, end).

init :-
	load_foreign,
	ignore(register_catalog('HTML4')).

:- initialization
	init.
	

		 /*******************************
		 *	   DTD HANDLING		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that concurrent access to DTD objects  is not allowed, and hence we
will allocate and destroy them in each   thread.  Possibibly it would be
nicer to find out why  concurrent  access   to  DTD's  is  flawed. It is
diagnosed to mess with the entity resolution by Fabien Todescato.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- thread_local
	current_dtd/2.
:- volatile
	current_dtd/2.

:- multifile
	dtd_alias/2.

dtd_alias(html, 'HTML4').

dtd(Type, DTD) :-
	current_dtd(Type, DTD), !.
dtd(Type, DTD) :-
	new_dtd(Type, DTD),
	(   dtd_alias(Type, Base)
	->  true
	;   Base = Type
	),
	absolute_file_name(dtd(Base),
			   [ extensions([dtd]),
			     access(read)
			   ], DtdFile),
	load_dtd(DTD, DtdFile),
	asserta(current_dtd(Type, DTD)).

%	load_dtd(+DTD, +DtdFile, +Options)
%	
%	Load file into a DTD.  Defined options are:
%	
%		# dialect(+Dialect)
%		Dialect to use (xml, xmlns, sgml)
%
%		# encoding(+Encoding)
%		Encoding of DTD file

load_dtd(DTD, DtdFile) :-
	load_dtd(DTD, DtdFile, []).
load_dtd(DTD, DtdFile, Options) :-
	split_dtd_options(Options, DTDOptions, FileOptions),
	open_dtd(DTD, DTDOptions, DtdOut),
	open(DtdFile, read, DtdIn, FileOptions),
	copy_stream_data(DtdIn, DtdOut),
	close(DtdIn),
	close(DtdOut).

split_dtd_options([], [], []).
split_dtd_options([H|T], [H|TD], S) :-
	dtd_option(H), !,
	split_dtd_options(T, TD, S).
split_dtd_options([H|T], TD, [H|S]) :-
	split_dtd_options(T, TD, S).

dtd_option(dialect(_)).


%	destroy_dtds
%	
%	Destroy  DTDs  cached  by  this  thread   as  they  will  become
%	unreachable anyway.

destroy_dtds :-
	(   current_dtd(_Type, DTD),
	    free_dtd(DTD),
	    fail
	;   true
	).

%	catch for if we do not have threads

:- catch(thread_at_exit(destroy_dtds), _, true).


		 /*******************************
		 *	    EXAMINE DTD		*
		 *******************************/

prop(doctype(_), _).
prop(elements(_), _).
prop(entities(_), _).
prop(notations(_), _).
prop(entity(E, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, entities(EL)),
	    member(E, EL)
	).
prop(element(E, _, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, elements(EL)),
	    member(E, EL)
	).
prop(attributes(E, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, elements(EL)),
	    member(E, EL)
	).
prop(attribute(E, A, _, _), DTD) :-
	(   nonvar(E)
	->  true
	;   '$dtd_property'(DTD, elements(EL)),
	    member(E, EL)
	),
	(   nonvar(A)
	->  true
	;   '$dtd_property'(DTD, attributes(E, AL)),
	    member(A, AL)
	).
prop(notation(N, _), DTD) :-
	(   nonvar(N)
	->  true
	;   '$dtd_property'(DTD, notations(NL)),
	    member(N, NL)
	).

dtd_property(DTD, Prop) :-
	prop(Prop, DTD),
	'$dtd_property'(DTD, Prop).


		 /*******************************
		 *	       SGML		*
		 *******************************/

parser_option(dialect(_)).
parser_option(shorttag(_)).
parser_option(file(_)).
parser_option(line(_)).
parser_option(space(_)).
parser_option(number(_)).
parser_option(defaults(_)).
parser_option(doctype(_)).
parser_option(qualify_attributes(_)).
parser_option(encoding(_)).

set_parser_options(Parser, Options, RestOptions) :-
	parser_option(Option),
	select_option(Option, Options, RestOptions0), !,
	set_sgml_parser(Parser, Option),
	set_parser_options(Parser, RestOptions0, RestOptions).
set_parser_options(_, Options, Options).


load_structure(stream(In), Term, Options) :- !,
	(   select_option(offset(Offset), Options, Options1)
	->  seek(In, Offset, bof, _)
	;   Options1 = Options
	),
	(   select_option(dtd(DTD), Options1, Options2)
	->  ExplicitDTD = true
	;   ExplicitDTD = false,
	    Options2 = Options1
	),
	new_sgml_parser(Parser,
			[ dtd(DTD)
			]),
	def_entities(Options2, DTD, Options3),
	call_cleanup(parse(Parser, Options3, TermRead, In),
		     free_sgml_parser(Parser)),
	(   ExplicitDTD == true
	->  (   DTD = dtd(_, DocType),
	        dtd_property(DTD, doctype(DocType))
	    ->	true
	    ;	true
	    )
	;   free_dtd(DTD)
	),
	Term = TermRead.
load_structure(Stream, Term, Options) :-
	is_stream(Stream), !,
	load_structure(stream(Stream), Term, Options).
load_structure(File, Term, Options) :-
	open(File, read, In, [type(binary)]),
	load_structure(stream(In), Term, [file(File)|Options]),
	close(In).

parse(Parser, Options, Document, In) :-
	set_parser_options(Parser, Options, Options1),
	sgml_parse(Parser,
		   [ document(Document),
		     source(In)
		   | Options1
		   ]).

def_entities([], _, []).
def_entities([entity(Name, Value)|T], DTD, Opts) :- !,
	def_entity(DTD, Name, Value),
	def_entities(T, DTD, Opts).
def_entities([H|T0], DTD, [H|T]) :-
	def_entities(T0, DTD, T).

def_entity(DTD, Name, Value) :-
	open_dtd(DTD, [], Stream),
	xml_quote_attribute(Value, QValue),
	format(Stream, '<!ENTITY ~w "~w">~n', [Name, QValue]),
	close(Stream).
	
	
		 /*******************************
		 *	     UTILITIES		*
		 *******************************/

load_sgml_file(File, Term) :-
	load_structure(File, Term, [dialect(sgml)]).

load_xml_file(File, Term) :-
	load_structure(File, Term, [dialect(xml)]).

load_html_file(File, Term) :-
	dtd(html, DTD),
	load_structure(File, Term,
		       [ dtd(DTD),
			 dialect(sgml),
			 shorttag(false)
		       ]).


		 /*******************************
		 *	      UTIL		*
		 *******************************/

%	select_option(Option(?Value), +OptionList, -RestList)
%	
%	Get  value  for  Option,  returning   the  unparsed  options  in
%	RestList.

select_option(Opt, Options, Rest) :-
	select(Opt, Options, Rest), !.
select_option(Opt, Options, Rest) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	select(OptName=OptVal, Options, Rest), !.


		 /*******************************
		 *	      ENCODING		*
		 *******************************/

%	xml_quote_attribute(+In, -Quoted)
%	xml_quote_cdata(+In, -Quoted)
%	
%	Backward  compatibility  for  versions  that  allow  to  specify
%	encoding. All characters that cannot fit the encoding are mapped
%	to XML character entities (&#dd;).  Using   ASCII  is the safest
%	value.

xml_quote_attribute(In, Quoted) :-
	xml_quote_attribute(In, Quoted, ascii).

xml_quote_cdata(In, Quoted) :-
	xml_quote_cdata(In, Quoted, ascii).

xml_name(In) :-
	xml_name(In, ascii).


		 /*******************************
		 *	   TYPE CHECKING	*
		 *******************************/

%	xml_is_dome(@Term)
%	
%	True  if  term  statisfies   the    structure   as  returned  by
%	load_structure/3 and friends.

xml_is_dom(0) :- !, fail.		% catch variables
xml_is_dom([]) :- !.
xml_is_dom([H|T]) :- !,
	xml_is_dom(H),
	xml_is_dom(T).
xml_is_dom(element(Name, Attributes, Content)) :- !,
	dom_name(Name),
	dom_attributes(Attributes),
	xml_is_dom(Content).
xml_is_dom(pi(Pi)) :- !,
	atom(Pi).
xml_is_dom(CDATA) :-
	atom(CDATA).

dom_name(NS:Local) :-
	atom(NS),
	atom(Local), !.
dom_name(Local) :-
	atom(Local).

dom_attributes(0) :- !, fail.
dom_attributes([]).
dom_attributes([H|T]) :-
	dom_attribute(H),
	dom_attributes(T).

dom_attribute(Name=Value) :-
	dom_name(Name),
	atomic(Value).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/
:- multifile
	prolog:message/3.

%	Catch messages.  sgml/4 is generated by the SGML2PL binding.

prolog:message(sgml(Parser, File, Line, Message)) -->
	{ get_sgml_parser(Parser, dialect(Dialect))
	},
	[ 'SGML2PL(~w): ~w:~w: ~w'-[Dialect, File, Line, Message] ].


		 /*******************************
		 *	   XREF SUPPORT		*
		 *******************************/

:- multifile
	prolog:called_by/2.

prolog:called_by(sgml_parse(_, Options), Called) :-
	findall(G+3, member(call(_, G), Options), Called).
