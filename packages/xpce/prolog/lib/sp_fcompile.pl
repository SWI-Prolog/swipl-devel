/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(sp_fcompile,
	  [ pce_fcompile/1
	  , pce_fcompile_boot_files/0
	  , pce_fcompile_directory/1
	  , pce_frecompile_directory/1
	  , pce_fcompile_libraries/0
	  , pce_frecompile_libraries/0
	  ]).
:- use_module(library(pce)).
:- require([ chain_list/2
	   , forall/2
	   , member/2
	   , absolute_file_name/3
	   ]).

:- prolog_flag(character_escapes, _, off).

		 /*******************************
		 *        TERM_EXPANSION	*
		 *******************************/

:- multifile
	user:term_expansion/2.
:- dynamic
	user:term_expansion/2.

user:term_expansion((:- List), (:- ensure_loaded(List))) :-
	List = [_|_].
user:term_expansion((:- consult(Files)), (:- ensure_loaded(Files))).
user:term_expansion((:- push_compile_operators), _) :-
	pce_expansion:push_compile_operators,
	fail.
user:term_expansion((:- pop_compile_operators), _) :-
	pce_expansion:pop_compile_operators,
	fail.

		 /*******************************
		 *	COMPILE STATEMENTS	*
		 *******************************/


pce_fcompile(File) :-
	use_module(user:File, []),
	fcompile(user:File).


dirpath(Dir, DirPath) :-
	absolute_file_name(Dir,
			   [ file_type(directory),
			     access(read)
			   ],
			   DirPath).


pce_fcompile(Dir, File) :-
	dirpath(Dir, DirPath),
	get(string('%s/%s', DirPath, File), value, Path),
	pce_fcompile(Path).


pce_frecompile(Dir, File) :-
	dirpath(Dir, DirPath),
	get(string('%s/%s', DirPath, File), value, PL),
	get(PL, delete_suffix, '.pl', Base),
	get(Base, ensure_suffix, '.ql', QL),
	(   (	\+ send(file(QL), exists)
	    |   send(file(PL)?time, after, file(QL)?time)
	    )
	->  pce_fcompile(PL)
	;   true
	).


pce_fcompile_directory(Dir) :-
	dirpath(Dir, DirPath),
	get(directory(DirPath), files, '^.*\\.pl$', Chain),
	send(Chain, delete_all, 'INDEX.pl'),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_fcompile(DirPath, File)).


pce_frecompile_directory(Dir) :-
	dirpath(Dir, DirPath),
	get(directory(DirPath), files, '^.*\\.pl$', Chain),
	send(Chain, delete_all, 'INDEX.pl'),
	chain_list(Chain, Files),
	forall(member(File, Files),
	       pce_frecompile(DirPath, File)).


pce_fcompile_libraries :-
	forall(pce_prolog_directory(LibDir),
	       pce_fcompile_directory(LibDir)).

pce_prolog_directory(pce(library)).
pce_prolog_directory(pce('library/draw')) :-
	ensure_loaded(user:library(pcedraw)).
pce_prolog_directory(pce('library/man')) :-
	ensure_loaded(user:library(pce_manual)).
pce_prolog_directory(pce('library/emacs')) :-
	ensure_loaded(user:library(pce_emacs)),
	user:start_emacs.
pce_prolog_directory(pce('library/dialog')) :-
	ensure_loaded(user:library(edit_dialog)).
pce_prolog_directory(pce('demo')).
pce_prolog_directory(pce('contrib')).

pce_frecompile_libraries :-
	forall(pce_prolog_directory(LibDir),
	       pce_frecompile_directory(LibDir)).

boot_file(pce_boot(pce_expand)).
boot_file(pce_boot(pce_sp)).
boot_file(pce_boot(pce_principal)).
boot_file(pce_boot(pce_error)).
boot_file(pce_boot(pce_operator)).
boot_file(pce_boot(pce_global)).
boot_file(pce_boot(pce_expansion)).
boot_file(pce_boot(pce_realise)).
boot_file(pce_boot(pce_autoload)).
boot_file(pce_boot(pce_editor)).
boot_file(library(sp_compatibility)).
boot_file(library(pce)).

pce_fcompile_boot_files :-
	forall(boot_file(BootFile),
	       fcompile(user:BootFile)).
