/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

:- module(prolog_hotfix,
	  [ load_hotfixes/1		% +Directory
	  ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(prolog_source)).

/** <module> Load hotfixes into executables

This library was developed to  deal   with  hotfixing  products that are
distributed as a Prolog saved state. It assumes the vendor is willing to
distribute hotfixes as Prolog source files.  These files are placed into
a directory. The  predicate  load_hotfixes/1   replaces  files  that are
loaded into the saved state.

Resolution of the file to load is based on the module if the hotfix file
provides a module. If the hotfix file is not a module file and there are
multiple  loaded  source  files  with  the   same  name  from  different
directories, the hotfix directory  must   create  the  minimal directory
structure to make the paths unique. If omitted, this library will prompt
the user for the file that must be replaced.

@tbd	This could be extended in several ways:

	    * Load hotfixes from a (encrypted) zip file
	    * Use digital signatures and load over HTTP
	    * Replace individual predicates

@author Jan Wielemaker
*/

%%	load_hotfixes(+Dir) is det.
%
%	Load all hotfixes that  have  not   yet  been  applied  into the
%	current state.

load_hotfixes(Dir) :-
	absolute_file_name(Dir, DirPath,
			   [ file_type(directory),
			     access(read)
			   ]),
	phrase(prolog_source_files([DirPath]), Files),
	ensure_dirsep(DirPath, Common),
	maplist(apply_hotfix(Common), Files).


%%	prolog_source_files(+Dirs)// is det.
%
%	Find all Prolog source files in the given directory.

prolog_source_files([]) --> !.
prolog_source_files([H|T]) --> !,
	prolog_source_files(H),
	prolog_source_files(T).
prolog_source_files(F) -->
	{ exists_file(F),
	  file_name_extension(_, Ext, F),
	  user:prolog_file_type(Ext, prolog)
	}, !,
	[F].
prolog_source_files(Dir) -->
	{ exists_directory(Dir), !,
	  atom_concat(Dir, '/*', Pattern),
	  expand_file_name(Pattern, Members)
	},
	prolog_source_files(Members).
prolog_source_files(_) -->
	[].


%%	apply_hotfix(+HotfixDir, +File) is det.
%
%	Locate the hotfix and load it if it is newer. First step to find
%	the file we must replace is using  the module name, as these are
%	guaranteed to be unique in the Prolog process. If that fails, we
%	use the filename, but now we  can   get  multiple files with the
%	same name loaded  from  different   directories  as  candidates.
%	Finally, if no file matches, we load   the  file into the =user=
%	module.

apply_hotfix(_HotfixDir, File) :-
	file_module(File, Module),
	module_property(Module, file(Loaded)),
	'$time_source_file'(Loaded, Time, _Type), !,
	time_file(File, HotfixTime),
	(   HotfixTime =\= Time
	->  load_hotfix(File, Loaded)
	;   true
	).
apply_hotfix(HotfixDir, File) :-
	atom_concat(HotfixDir, Local, File),
	atom_concat(/, Local, SlashLocal),
	findall(Loaded-Time,
		(   '$time_source_file'(Loaded, Time, user),
		    sub_atom(Loaded, _, _, 0, SlashLocal)
		),
		Pairs),
	Pairs \== [], !,
	(   Pairs = [Loaded-Time]
	->  true
	;   select_file_to_reload(Pairs, Local, Loaded-Time)
	),
	time_file(File, HotfixTime),
	(   HotfixTime =\= Time
	->  load_hotfix(File, Loaded)
	;   true
	).
apply_hotfix(_HotfixDir, File) :-
	user:consult(File).


%%	ensure_dirsep(+Dir, -DirSlash) is det.

ensure_dirsep(Dir0, Dir) :-
	(   sub_atom(Dir0, _, _, 0, /)
	->  Dir = Dir0
	;   atom_concat(Dir0, /, Dir)
	).


%%	load_hotfix(+HotfixFile, +Loaded) is det.
%
%	Reload the HotfixFile, pretending we are reloading Loaded.
%
%	@see	make:reload_file/1

load_hotfix(File, Loaded) :-
	time_file(File, Modified),
	setup_call_cleanup(
	    open(File, read, In),
	    load_hotfix_from_stream(Loaded, In, Modified),
	    close(In)).

load_hotfix_from_stream(Loaded, In, Modified) :-
	FixOptions = [ stream(In),
		       modified(Modified),
		       register(false)
		     ],
	set_stream(In, file_name(Loaded)),
	findall(M-Opts,
		source_file_property(Loaded, load_context(M, _, Opts)),
		Modules),
	(   Modules = [First-OptsFirst|Rest]
	->  merge_options(FixOptions, OptsFirst, FirstOptions),
	    load_stream(First:Loaded, FirstOptions),
	    forall(member(Context-Opts, Rest),
		   ( merge_options([if(not_loaded)|FirstOptions], Opts, ORest),
		     load_stream(Context:Loaded, ORest)
		   ))
	;   load_stream(user:Loaded, FixOptions)
	).

load_stream(Source, Options) :-
	option(stream(In), Options),
	setup_call_cleanup(
	    stream_property(In, position(Pos)),
	    load_files(Source, Options),
	    set_stream_position(In, Pos)).

%%	select_file_to_reload(+Pairs, +Local, -Pair) is det.

select_file_to_reload(Pairs, Local, Pair) :-
	format(user_error,
	       'Hotfix ~w matches multiple loaded files.~n~n',
	       [Local]),
	forall(nth1(I, Pairs, File-_),
	       format(user_error, '~t~d~6| ~w~n', [I, File])),
	repeat,
	   format(user_error, '~nPlease select (\'s\' skips hotfix)? ', []),
	   read_line_to_codes(user_input, Line),
	   (   Line == end_of_file
	   ->  halt(1)
	   ;   Line == "s"
	   ->  !, fail
	   ;   catch(number_codes(N, Line), _, fail)
	   ),
	   nth1(N, Pairs, Pair), !.

%%	file_module(+File, -Module) is semidet.
%
%	True if Module is the module defined in File.

file_module(File, Module) :-
	catch(file_module_guarded(File, Module), _, fail).

file_module_guarded(File, Module) :-
	setup_call_cleanup(
	    prolog_open_source(File, In),
	    prolog_read_source_term(In, _, Expanded, []),
	    prolog_close_source(In)),
	Expanded = (:- module(Module, _)).
