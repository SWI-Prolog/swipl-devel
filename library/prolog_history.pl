/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2017, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(prolog_history,
          [ prolog_history/1
          ]).
:- use_module(library(base32)).

:- multifile
    prolog:history/2.

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

%!  history_directory(-Dir) is semidet.
%
%   Dir is the directory where   the per-directory history databases
%   are stored.

history_directory(Dir) :-
    absolute_file_name(app_preferences('.swipl-dir-history'),
                       Dir,
                       [ access(write),
                         file_type(directory),
                         file_errors(fail)
                       ]),
    !.
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

%!  dir_history_file(+Dir, -File) is det.
%!  dir_history_file(?Dir, ?File) is nondet.
%
%   File is the history file for a Prolog session running in Dir.

dir_history_file(Dir, File) :-
    nonvar(Dir),
    !,
    history_directory(Base),
    absolute_file_name(Dir, Path),
    base32(Path, Encoded),
    atomic_list_concat([Base, Encoded], /, File).
dir_history_file(Dir, File) :-
    history_directory(HDir),
    directory_files(HDir, Files),
    '$member'(Base32, Files),
    base32(Dir, Base32),
    !,
    atomic_list_concat([Dir, Base32], /, File).

write_history(File) :-
    current_prolog_flag(save_history, true),
    catch(prolog:history(user_input, save(File)), _, true), !.
write_history(_).


%!  prolog_history(+Action) is det.
%
%   Execute Action on  the  history.   Action is one of
%
%     * enable
%     Enable history. First loads history for the current directory.
%     Loading the history is done at most once.
%     * disable
%     Sets the Prolog flag =save_history= to =false=, such that the
%     history is not saved on halt.

:- dynamic
    history_loaded/1.

load_dir_history(File) :-
    (   exists_file(File),
        prolog:history(user_input, load(File))
    ->  assertz(history_loaded(File))
    ;   true
    ).

prolog_history(enable) :-
    history_loaded(_),
    !.
prolog_history(enable) :-
    catch(dir_history_file('.', File), E,
          (print_message(warning, E),fail)),
    catch(load_dir_history(File), E,
          print_message(warning, E)),
    !,
    at_halt(write_history(File)),
    set_prolog_flag(save_history, true).
prolog_history(_) :-
    set_prolog_flag(save_history, false).

		 /*******************************
		 *      SWIPL-WIN SUPPORT	*
		 *******************************/

:- if(current_predicate('$rl_history'/1)).
:- use_module(library(readutil)).

prolog:history(_, load(File)) :-
    access_file(File, read),
    !,
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        read_history(In),
        close(In)).
prolog:history(_, load(_)).

read_history(In) :-
    repeat,
    read_line_to_codes(In, Codes),
    (   Codes == end_of_file
    ->  !
    ;   atom_codes(Line, Codes),
        rl_add_history(Line),
        fail
    ).

prolog:history(_, save(File)) :-
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
