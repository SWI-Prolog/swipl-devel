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

:- module(sgml,
	  [ load_sgml_file/2,		% +File, -ListOfContent
	    load_xml_file/2,		% +File, -ListOfContent
	    load_html_file/2,		% +File, -Document

	    load_structure/3,		% +File, -Term, +Options

	    load_dtd/2,			% +DTD, +File
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

	    xml_quote_attribute/2,	% +In, -Quoted
	    xml_quote_cdata/2,		% +In, -Quoted
	    xml_name/1			% +In
	  ]).

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

load_dtd(DTD, DtdFile) :-
	open_dtd(DTD, [], DtdOut),
	open(DtdFile, read, DtdIn),
	copy_stream_data(DtdIn, DtdOut),
	close(DtdIn),
	close(DtdOut).

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

:- thread_at_exit(destroy_dtds).


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
	call_cleanup(parse(Parser, Options2, TermRead, In),
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
	Stream = '$stream'(_), !,
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
			 dialect(sgml)
		       ]).


		 /*******************************
		 *	      UTIL		*
		 *******************************/

%	option(Option(?Value), OptionList, Default)

option(Opt, Options) :-
	memberchk(Opt, Options), !.
option(Opt, Options) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	memberchk(OptName=OptVal, Options), !.

select_option(Opt, Options, Rest) :-
	select(Opt, Options, Rest), !.
select_option(Opt, Options, Rest) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	select(OptName=OptVal, Options, Rest), !.

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
