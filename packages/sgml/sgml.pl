/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(sgml,
	  [ new_dtd/2,			% +Doctype, -DTD
	    free_dtd/1,			% +DTD
	    new_sgml_parser/2,		% -Parser, +Options
	    free_sgml_parser/1,		% +Parser

	    sgml_open/3,		% +DTD|Parser, +Options, -OutStream
	    
	    load_dtd/2,			% +DTD, +File
	    dtd/2,			% +Type, -DTD
	    dtd_property/2,		% +DTD, ?Property

	    load_structure/3,		% +File, -Term, +Options

	    load_sgml_file/2,		% +File, -Document
	    load_sgml_file/3,		% +File, -Document, ?DTD
	    load_html_file/2		% +File, -Document
	  ]).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(dtd, '.').

load_foreign :-
	load_foreign_library(foreign(sgml2pl)).

:- initialization load_foreign.
	
		 /*******************************
		 *	   DTD HANDLING		*
		 *******************************/

:- dynamic
	current_dtd/2.
:- volatile
	current_dtd/2.

dtd(Type, DTD) :-
	current_dtd(Type, DTD), !.
dtd(Type, DTD) :-
	new_dtd(Type, DTD),
	absolute_file_name(dtd(Type),
			   [ extensions([dtd]),
			     access(read)
			   ], DtdFile),
	load_dtd(DTD, DtdFile),
	asserta(current_dtd(Type, DTD)).

load_dtd(DTD, DtdFile) :-
	sgml_open(DTD, [], DtdOut),
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


dtd_property(DTD, Prop) :-
	prop(Prop, DTD),
	'$dtd_property'(DTD, Prop).


		 /*******************************
		 *	       SGML		*
		 *******************************/

parser_option(dialect(_)).
parser_option(file(_)).
parser_option(line(_)).

set_parser_options(Parser, Options, RestOptions) :-
	parser_option(Option),
	select_option(Option, Options, RestOptions0), !,
	set_sgml_parser(Parser, Option),
	set_parser_options(Parser, RestOptions0, RestOptions).
set_parser_options(_, Options, Options).


load_structure(File, Term, Options) :-
	open(File, read, In, [type(binary)]),
	(   select_option(dtd(DTD), Options, Options1)
	->  ExplicitDTD = true
	;   ExplicitDTD = false,
	    Options1 = Options
	),
	new_sgml_parser(Parser,
			[ dtd(DTD)
			]),
	set_sgml_parser(Parser, file(File)),
	set_parser_options(Parser, Options1, Options2),
	sgml_open(Parser,
		  [ document(Term),
		    goal(copy_stream_data(In, Out))
		  | Options2
		  ],
		  Out),
	close(In),
	close(Out),
	(   ExplicitDTD == true
	->  (   DTD = dtd(_, DocType),
	        dtd_property(DTD, doctype(DocType))
	    ->	true
	    ;	true
	    )
	;   free_dtd(DTD)
	).


load_sgml_file(File, Term, DTD) :-
	open(File, read, SgmlIn),
	new_sgml_parser(Parser, [dtd(DTD)]),
	set_sgml_parser(Parser, file(File)),
	sgml_open(Parser,
		  [ document(Term),
		    goal(copy_stream_data(SgmlIn, SgmlOut))
		  ],
		  SgmlOut),
	close(SgmlIn),
	close(SgmlOut),
	(   DTD = dtd(_, DocType),
	    dtd_property(DTD, doctype(DocType))
	->  true
	;   true
	).

load_sgml_file(File, Term) :-
	open(File, read, SgmlIn),
	new_sgml_parser(Parser, [dtd(DTD)]),
	set_sgml_parser(Parser, file(File)),
	sgml_open(Parser,
		  [ document(Term),
		    goal(copy_stream_data(SgmlIn, SgmlOut))
		  ],
		  SgmlOut),
	close(SgmlIn),
	close(SgmlOut),
	free_dtd(DTD).


		 /*******************************
		 *	 HTML (UTILITIES)	*
		 *******************************/

load_html_file(File, Term) :-
	dtd(html, DTD),
	load_sgml_file(File, Term, DTD).


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

option(Opt, Options, _) :-
	option(Opt, Options), !.
option(Opt, _, Default) :-
	arg(1, Opt, Default).

select_option(Opt, Options, Rest) :-
	select(Options, Opt, Rest), !.
select_option(Opt, Options, Rest) :-
	functor(Opt, OptName, 1),
	arg(1, Opt, OptVal),
	select(Options, OptName=OptVal, Rest), !.

