/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2011, VU University Amsterdam

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

:- module(prolog_history,
	  [ prolog_history/1
	  ]).
:- use_module(library(base32)).
:- use_module(library(lists)).

/** <module> Per-directory persistent commandline history

This module implements  persistency  of   the  commandline  history over
Prolog sessions on Prolog  installations  that   are  based  on  the GNU
readline library (default for the development version on Unix systems).

The history is stored  in   the  directory =|~/.swipl-dir-history|=. For
each directory for which it keeps the  history, there is file whose name
is the base32 encoding of the directory path.

This file is normally loaded when Prolog is started if =user_input= is a
terminal and the system supports history.
*/

:- create_prolog_flag(save_history, true, [type(boolean)]).

%%	history_directory(-Dir) is semidet.
%
%	Dir is the directory where   the per-directory history databases
%	are stored.

history_directory(Dir) :-
	absolute_file_name(app_preferences('.swipl-dir-history'),
			   Dir,
			   [ access(write),
			     file_type(directory),
			     file_errors(fail)
			   ]), !.
history_directory(Dir) :-
	absolute_file_name(app_preferences('.'),
			   Home,
			   [ access(write),
			     file_type(directory),
			     file_errors(fail)
			   ]),
	atom_concat(Home, '/.swipl-dir-history', Dir),
	(   exists_directory(Dir)
	->  fail
	;   make_directory(Dir)
	).

%%	dir_history_file(+Dir, -File) is det.
%%	dir_history_file(?Dir, ?File) is nondet.
%
%	File is the history file for a Prolog session running in Dir.

dir_history_file(Dir, File) :-
	nonvar(Dir), !,
	history_directory(Base),
	absolute_file_name(Dir, Path),
	base32(Path, Encoded),
	atomic_list_concat([Base, Encoded], /, File).
dir_history_file(Dir, File) :-
	history_directory(HDir),
	directory_files(HDir, Files),
	member(Base32, Files),
	base32(Dir, Base32),
	atomic_list_concat([Dir, Base32], /, File).

% Realise write/read of history for the swipl-win.exe console.

:- if((\+current_predicate(rl_read_history/1),
       current_predicate('$rl_history'/1))).
:- use_module(library(readutil)).

system:rl_read_history(File) :-
	access_file(File, read), !,
	setup_call_cleanup(
	    open(File, read, In, [encoding(utf8)]),
	    read_history(In),
	    close(In)).
system:rl_read_history(_).

read_history(In) :-
	repeat,
	read_line_to_codes(In, Codes),
	(   Codes == end_of_file
	->  !
	;   atom_codes(Line, Codes),
	    rl_add_history(Line),
	    fail
	).

system:rl_write_history(File) :-
	'$rl_history'(Lines),
	(   Lines \== []
	->  setup_call_cleanup(
		open(File, write, Out, [encoding(utf8)]),
		forall(member(Line, Lines),
		       format(Out, '~w~n', [Line])),
		close(Out))
	;   true
	).

:- endif.

:- if(current_predicate(rl_write_history/1)).
write_history(File) :-
	current_prolog_flag(save_history, true), !,
	rl_write_history(File).
:- endif.
write_history(_).


%%	prolog_history(+Action) is det.
%
%	Execute Action on  the  history.   Action is one of
%
%	  * enable
%	  Enable history. First loads history for the current directory.
%	  Loading the history is done at most once.
%	  * disable
%	  Sets the Prolog flag =save_history= to =false=, such that the
%	  history is not saved on halt.

:- if(current_predicate(rl_read_history/1)).
:- dynamic
	history_loaded/1.
prolog_history(enable) :-
	history_loaded(_), !.
prolog_history(enable) :- !,
	dir_history_file('.', File),
	(   exists_file(File)
	->  rl_read_history(File),
	    assertz(history_loaded(File))
	;   true
	),
	at_halt(write_history(File)),
	set_prolog_flag(save_history, true).
:- endif.
prolog_history(_) :-
	set_prolog_flag(save_history, false).
