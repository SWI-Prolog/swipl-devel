/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module($qlf,
	  [ qcompile/1,		% +File
	    $qload_file/6	% +Path, +Module, +Import, +IsModule, -Ac, -LM
	  ]).


		 /*******************************
		 *	   COMPILATION		*
		 *******************************/

:- module_transparent
	qcompile/1.

qcompile([]) :- !.
qcompile([H|T]) :- !,
	qcompile(H),
	qcompile(T).
qcompile(File) :-
	$strip_module(File, Module, FileName),
	absolute_file_name(FileName,
			   [ file_type(prolog),
			     access(read)
			   ], Absolute),
	file_name_extension(ABase, PlExt, Absolute),
	user:prolog_file_type(PlExt, prolog),
	user:prolog_file_type(QlfExt, qlf),
	file_name_extension(ABase, QlfExt, Qlf),
	$qlf_open(Qlf),
	flag($compiling, Old, qlf),
	$set_source_module(OldModule, Module), % avoid this in the module!
	(   consult(Module:Absolute)
	->  Ok = true
	;   Ok = fail
	),
	$set_source_module(_, OldModule),
	flag($compiling, _, Old),
	$qlf_close,
	Ok == true.


%	qload(+File, +Module, +Import, -IsModule, -Action, -LoadedModule)
%
%	Load predicate for .qlf files.  See init.pl

$qload_file(File, Module, Import, IsModule, loaded, LoadedModule) :-
	$qlf_load(Module:File, LoadedModule),
	check_is_module(IsModule, LoadedModule, File),
	(   atom(LoadedModule)
	->  $import_list(Module, LoadedModule, Import)
	;   true
	).
	

check_is_module(true, 0, File) :- !,
	throw(error(domain_error(module_file, File), _)).
check_is_module(_, _, _).
