/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
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

	    sgml_register_catalog_file/2 % +File, +StartOrEnd
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

:- dynamic
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
parser_option(file(_)).
parser_option(line(_)).
parser_option(space(_)).
parser_option(number(_)).

set_parser_options(Parser, Options, RestOptions) :-
	parser_option(Option),
	select_option(Option, Options, RestOptions0), !,
	set_sgml_parser(Parser, Option),
	set_parser_options(Parser, RestOptions0, RestOptions).
set_parser_options(_, Options, Options).


load_structure(File, Term, Options) :-
	open(File, read, In, [type(binary)]),
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
	set_sgml_parser(Parser, file(File)),
	set_parser_options(Parser, Options2, Options3),
	sgml_parse(Parser,
		  [ document(Term),
		    source(In)
		  | Options3
		  ]),
	close(In),
	(   ExplicitDTD == true
	->  (   DTD = dtd(_, DocType),
	        dtd_property(DTD, doctype(DocType))
	    ->	true
	    ;	true
	    )
	;   free_dtd(DTD)
	).


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
