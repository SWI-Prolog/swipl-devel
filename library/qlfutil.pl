/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(qlfutil,
	  [ qlf_make_dir/1,
	    qlf_make/1
	  ]).

qlf_make_dir(Dir) :-
	absolute_file_name('', Cwd),
	chdir(Dir),
	expand_file_name('*.qlf', QlfFiles),
	forall(member(Qlf, QlfFiles),
	       qlf_make(Qlf)),
	chdir(Cwd).

qlf_make(Base) :-
	absolute_file_name(Base,
			   [ extensions(['.qlf']),
			     access(read)
			   ],
			   QlfFile),
	'$qlf_info'(QlfFile, V, V, Sources), !,
	(   time_file(QlfFile, QlfStamp),
	    \+ ( member(Source, Sources),
		 time_file(Source, SourceStamp),
		 SourceStamp @> QlfStamp )
	->  true
	;   (   forall(member(Source, Sources),
		       access_file(Source, read))
	    ->	concat(PlBase, '.qlf', QlfFile),
	        user:qcompile(PlBase)
	    ;	'$warning'('Cannot update ~w: no access to sourcefile ~w',
			   [ QlfFile, Source ])
	    )
	).
qlf_make(Base) :-
	qcompile(Base).
	
	
