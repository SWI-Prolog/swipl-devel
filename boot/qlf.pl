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
