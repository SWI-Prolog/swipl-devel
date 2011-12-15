/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module('$qlf',
	  [ qcompile/1,		% :Files
	    qcompile/2,		% :Files, +Options
	    '$qload_file'/5,	% +Path, +Module, -Ac, -LM, +Options
	    '$qload_stream'/5	% +Stream, +Module, -Ac, -LM, +Options
	  ]).


		 /*******************************
		 *	   COMPILATION		*
		 *******************************/

:- meta_predicate
	qcompile(:),
	qcompile(:, +).

%%	qcompile(:Files) is det.
%
%	Compile Files as consult/1 and generate   a  Quick Load File for
%	each compiled file.

qcompile(M:Files) :-
	qcompile_(Files, M, []).
qcompile(M:Files, Options) :-
	qcompile_(Files, M, Options).

qcompile_([], _, _) :- !.
qcompile_([H|T], M, Options) :- !,
	qcompile_(H, M, Options),
	qcompile_(T, M, Options).
qcompile_(FileName, Module, Options) :-
	absolute_file_name(FileName,
			   [ file_type(prolog),
			     access(read)
			   ], Absolute),
	file_name_extension(ABase, PlExt, Absolute),
	(   user:prolog_file_type(PlExt, qlf)
	->  throw(error(permission_error(compile, qlf, FileName),
			context(qcompile/1, 'Conflicting extension')))
	;   true
	),
	once(user:prolog_file_type(QlfExt, qlf)),
	file_name_extension(ABase, QlfExt, Qlf),
	load_files(Module:Absolute, ['$qlf'(Qlf)|Options]).


%%	'$qload_file'(+File, +Module, -Action, -LoadedModule, +Options)
%
%	Load predicate for .qlf files.  See init.pl

'$qload_file'(File, Module, Action, LoadedModule, Options) :-
	open(File, read, In, [type(binary)]),
	'$save_lex_state'(LexState),
	call_cleanup('$qload_stream'(In, Module,
				     Action, LoadedModule, Options),
		     (	 close(In),
			 '$restore_lex_state'(LexState)
		     )).


'$qload_stream'(In, Module, loaded, LoadedModule, Options) :-
	'$qlf_load'(Module:In, LM),
	check_is_module(LM, In, Options),
	(   atom(LM)
	->  LoadedModule = LM
	;   LoadedModule = Module
	).

check_is_module(LM, In, Options) :-
	\+ atom(LM),
	'$get_option'(must_be_module(true), Options, false), !,
	stream_property(In, file_name(File)),
	throw(error(domain_error(module_file, File), _)).
check_is_module(_, _, _).
