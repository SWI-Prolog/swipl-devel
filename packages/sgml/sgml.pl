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

