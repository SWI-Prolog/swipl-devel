/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module($qlf,
	  [ qcompile/1,		% +File
	    qload/1,		% +File
	    $qload_file/6	% +Path, +Module, +Import, +IsModule, -Ac, -LM
	  ]).


		 /*******************************
		 *	   COMPILATION		*
		 *******************************/

:- module_transparent
	qcompile/1,
	qload/1,
	qload/2.


qcompile([]) :- !.
qcompile([H|T]) :- !,
	qcompile(H),
	qcompile(T).
qcompile(File) :-
	$strip_module(File, Module, FileName),
	$chk_file(FileName, ['.pl', ''], Absolute), !,
	remove_suffix(Absolute, '.pl', ABase),
	ensure_suffix(ABase, '.qlf', Qlf),
	$qlf_open(Qlf),
	flag($compiling, Old, qlf),
	consult(Module:Absolute),
	flag($compiling, _, Old),
	$qlf_close.

remove_suffix(F, S, B) :-
	concat(B, S, F), !.
remove_suffix(F, _, F).

ensure_suffix(X, S, X) :-
	concat(_, S, X), !.
ensure_suffix(X, S, XS) :-
	concat(X, S, XS).

	
		 /*******************************
		 *	      LOADING		*
		 *******************************/

qload([]) :- !.
qload([H|T]) :- !,
	qload(H),
	qload(T).
qload(File) :-
	qload(File, [verbose = true]).

qload(File, Options) :-
	statistics(heapused, OldHeap),
	statistics(cputime, OldTime),

	$strip_module(File, Module, FileName),
	$chk_file(FileName, ['.qlf', ''], Absolute), !,
	$qlf_load(Module:Absolute, LoadedModule),
	(   atom(LoadedModule)
	->  (   memberchk(import = Import, Options)
	    ->	true
	    ;	Import = all
	    ),
	    $import_list(Module, LoadedModule, Import)
	;   true
	),

	(   memberchk(verbose=true, Options)
	->  statistics(heapused, Heap),
	    statistics(cputime, Time),
	    HeapUsed is Heap - OldHeap,
	    TimeUsed is Time - OldTime,
	    $confirm_file(FileName, Absolute, ConfirmFile),
	    $confirm_module(LoadedModule, ConfirmModule),

	    $ttyformat('~N~w loaded~w, ~2f sec, ~D bytes.~n',
		       [ConfirmFile, ConfirmModule, TimeUsed, HeapUsed])
	;   true
	).


$qload_file(File, Module, Import, IsModule, loaded, LoadedModule) :-
	$qlf_load(Module:File, LoadedModule),
	check_is_module(IsModule, LoadedModule, File),
	(   atom(LoadedModule)
	->  $import_list(Module, LoadedModule, Import)
	;   true
	).
	

check_is_module(true, 0, File) :- !,
	$warning('use_module: ~w is not a module file', [File]).
check_is_module(_, _, _).
