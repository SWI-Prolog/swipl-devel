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

:- module($autoload,
	  [ $find_library/5,
	    $in_library/2,
	    $define_predicate/1,
	    $update_library_index/0,
	    make_library_index/1,
	    make_library_index/2,
	    reload_library_index/0,
	    autoload/0,
	    autoload/1
	  ]).

:- dynamic
	library_index/3,		% Head x Module x Path
	autoload_directories/1.		% List
:- volatile
	library_index/3,
	autoload_directories/1.

%	$find_library(+Module, +Name, +Arity, -LoadModule, -Library)
%
%	Locate a predicate in the library.  Name and arity are the name
%	and arity of the predicate searched for.  `Module' is the
%	preferred target module.  The return values are the full path names
%	of the library and module declared in that file.

$find_library(Module, Name, Arity, LoadModule, Library) :-
	load_library_index(Name, Arity),
	functor(Head, Name, Arity),
	(   library_index(Head, Module, Library),
	    LoadModule = Module
	;   library_index(Head, LoadModule, Library)
	), !.

%	$in_library(?Name, ?Arity)
%
%	Is true if Name/Arity is in the autoload libraries.

$in_library(Name, Arity) :-
	atom(Name), integer(Arity), !,
	load_library_index(Name, Arity),
	functor(Head, Name, Arity),
	library_index(Head, _, _).
$in_library(Name, Arity) :-
	load_library_index,
	library_index(Head, _, _),
	functor(Head, Name, Arity).

%	$define_predicate(+Head)
%
%	Make sure pred can be called.  First test if the predicate is
%	defined.  If not, invoke the autoloader.

:- module_transparent
	$define_predicate/1.

$define_predicate(Head) :-
	$defined_predicate(Head), !.
$define_predicate(Term) :-
	$strip_module(Term, Module, Head),
	functor(Head, Name, Arity),
	current_prolog_flag(autoload, true),
	$find_library(Module, Name, Arity, LoadModule, Library),
	flag($autoloading, Old, Old+1),
	(   Module == LoadModule
	->  ignore(ensure_loaded(Library))
	;   ignore(Module:use_module(Library, [Name/Arity]))
	),
	flag($autoloading, _, Old),
	$define_predicate(Term).


		/********************************
		*          UPDATE INDEX		*
		********************************/

$update_library_index :-
	setof(Dir, indexed_directory(Dir), Dirs), !,
	checklist(guarded_make_library_index, Dirs),
	(   flag($modified_index, true, false)
	->  reload_library_index
	;   true
	).
$update_library_index.

guarded_make_library_index(Dir) :-
	catch(make_library_index(Dir), E,
	      print_message(error, E)), !.
guarded_make_library_index(Dir) :-
	print_message(warning, goal_failed(make_library_index(Dir))).


indexed_directory(Dir) :-
	index_file_name(IndexFile, [access(read), access(write)]),
	file_directory_name(IndexFile, Dir).
indexed_directory(Dir) :-
	absolute_file_name(library('MKINDEX'),
			   [ file_type(prolog),
			     access(read),
			     solutions(all),
			     file_errors(fail)
			   ], MkIndexFile),
	file_directory_name(MkIndexFile, Dir),
	plfile_in_dir(Dir, 'INDEX', _, IndexFile),
	access_file(IndexFile, write).


		/********************************
		*           LOAD INDEX		*
		********************************/

%	reload_library_index
%	
%	Reload the index on the next call

reload_library_index :-
	retractall(library_index(_, _, _)),
	retractall(autoload_directories(_)).


%	load_library_index(+Name, +Arity)
%	
%	Try to find Name/Arity  in  the   library.  If  the predicate is
%	there, we are happy. If not, we  check whether the set of loaded
%	libraries has changed and if so we reload the index.

load_library_index(Name, Arity) :-
	functor(Head, Name, Arity),
	library_index(Head, _, _), !.
load_library_index(_, _) :-
	findall(Index, index_file_name(Index, [access(read)]), List),
	(   autoload_directories(List)
	->  true
	;   retractall(library_index(_, _, _)),
	    retractall(autoload_directories(_)),
	    read_index(List),
	    assert(autoload_directories(List))
	).

%	load_library_index
%	
%	Load the library index if it was not loaded.

load_library_index :-
	library_index(_, _, _), !.		% loaded
load_library_index :-
	findall(Index, index_file_name(Index, [access(read)]), List),
	read_index(List),
	assert(autoload_directories(List)).

index_file_name(IndexFile, Options) :-
	absolute_file_name(library('INDEX'),
			   [ file_type(prolog),
			     solutions(all),
			     file_errors(fail)
			   | Options
			   ], IndexFile).

read_index([]) :- !.
read_index([H|T]) :- !,
	read_index(H),
	read_index(T).
read_index(Index) :-
	print_message(silent, autoload(read_index(Dir))),
	file_directory_name(Index, Dir),
	seeing(Old), see(Index),
	repeat,
	    read(Term),
	    assert_index(Term, Dir), !,
	seen, see(Old).

assert_index(end_of_file, _) :- !.
assert_index(index(Name, Arity, Module, File), Dir) :- !,
	functor(Head, Name, Arity),
	concat_atom([Dir, '/', File], Path),
	assertz(library_index(Head, Module, Path)),
	fail.
assert_index(Term, Dir) :-
	print_message(error, illegal_autoload_index(Dir, Term)),
	fail.
	

		/********************************
		*       CREATE INDEX.pl		*
		********************************/

make_library_index(Dir) :-
	make_library_index2(Dir).

make_library_index(Dir, Patterns) :-
	make_library_index2(Dir, Patterns).

make_library_index2(Dir) :-
	plfile_in_dir(Dir, 'MKINDEX', MkIndex, AbsMkIndex),
	access_file(AbsMkIndex, read), !,
	working_directory(OldDir, Dir),
	call_cleanup(load_files(user:MkIndex, [silent(true)]),
		     working_directory(_, OldDir)).
make_library_index2(Dir) :-
	findall(Pattern, source_file_pattern(Pattern), PatternList),
	make_library_index2(Dir, PatternList).
	
make_library_index2(Dir, Patterns) :-
	plfile_in_dir(Dir, 'INDEX', Index, AbsIndex),
	access_file(AbsIndex, write), !,
	working_directory(OldDir, Dir),
	working_directory(NewDir, NewDir),
	expand_index_file_patterns(Patterns, Files),
	(   library_index_out_of_date(Index, Files)
	->  print_message(informational, make(library_index(NewDir))),
	    flag($modified_index, _, true),
	    call_cleanup(do_make_library_index(Index, Files),
			 working_directory(_, OldDir))
	;   working_directory(_, OldDir)
	).
make_library_index2(Dir, _) :-
	throw(error(permission_error(write, index_file, Dir), _)).

source_file_pattern(Pattern) :-
	user:prolog_file_type(PlExt, prolog),
	atom_concat('*.', PlExt, Pattern).

plfile_in_dir(Dir, Base, PlBase, File) :-
	once(user:prolog_file_type(PlExt, prolog)),
	file_name_extension(Base, PlExt, PlBase),
	concat_atom([Dir, '/', PlBase], File).

expand_index_file_patterns(Patterns, Files) :-
	phrase(files_from_patterns(Patterns), Files).

files_from_patterns([]) -->
	[].
files_from_patterns([P0|PT]) -->
	{ expand_file_name(P0, Files) 
	},
	Files,
	files_from_patterns(PT).


library_index_out_of_date(Index, _Files) :-
	\+ exists_file(Index), !.
library_index_out_of_date(Index, Files) :-
	time_file(Index, IndexTime),
	(   time_file('.', DotTime),
	    DotTime @> IndexTime
	;   $member(File, Files),
	    time_file(File, FileTime),
	    FileTime @> IndexTime
	), !.


do_make_library_index(Index, Files) :-
	$style_check(OldStyle, OldStyle),
	style_check(-dollar),
	open(Index, write, Fd),
	index_header(Fd),
	checklist(index_file(Fd), Files),
	close(Fd),
	$style_check(_, OldStyle).

index_file(Fd, File) :-
	open(File, read, In),
	read(In, Term),
	close(In),
	Term = (:- module(Module, Public)), !,
	file_name_extension(Base, _, File),
	forall( $member(Name/Arity, Public),
		format(Fd, 'index((~k), ~k, ~k, ~k).~n',
		       [Name, Arity, Module, Base])).
index_file(_, _).

index_header(Fd):-
	format(Fd, '/*  $Id', []),
	format(Fd, '$~n~n', []),
	format(Fd, '    Creator: make/0~n~n', []),
	format(Fd, '    Purpose: Provide index for autoload~n', []),
	format(Fd, '*/~n~n', []).

		 /*******************************
		 *	   DO AUTOLOAD		*
		 *******************************/

%	autoload([options ...]) 
%
%	Force all necessary autoloading to be done now.

autoload :-
	autoload([]).

autoload(Options) :-
	option(Options, verbose/true, Verbose),
	$style_check(Old, Old), 
	style_check(+dollar), 
	current_prolog_flag(autoload, OldAutoLoad),
	current_prolog_flag(verbose_autoload, OldVerbose),
	set_prolog_flag(autoload, false),
	findall(Pred, needs_autoloading(Pred), Preds),
	set_prolog_flag(autoload, OldAutoLoad),
	$style_check(_, Old),
	(   Preds == []
	->  true
	;   set_prolog_flag(autoload, true),
	    set_prolog_flag(verbose_autoload, Verbose),
	    checklist($define_predicate, Preds),
	    set_prolog_flag(autoload, OldAutoLoad),
	    set_prolog_flag(verbose_autoload, OldVerbose),
	    autoload(Verbose)		% recurse for possible new
					% unresolved links
	).
	
needs_autoloading(Module:Head) :-
	predicate_property(Module:Head, undefined), 
	\+ predicate_property(Module:Head, imported_from(_)), 
	functor(Head, Functor, Arity), 
	$in_library(Functor, Arity).

option(Options, Name/Default, Value) :-
	(   memberchk(Name = Value, Options)
	->  true
	;   Value = Default
	).

