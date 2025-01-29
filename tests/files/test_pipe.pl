/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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



:- module(test_pipe,
          [ test_pipe/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_pipe :-
    run_tests([ pipe
              ]).

:- begin_tests(pipe,
               [ condition(current_prolog_flag(pipe,true)),
                 sto(rational_trees)    % rational_trees: only run once
               ]).

wine :-
    current_prolog_flag(wine_version, _).

test(pwd, [condition(not(wine))]) :-
    (   current_prolog_flag(windows, true)
    ->  Command = 'cmd /c cd'
    ;   Command = pwd
    ),
    setup_call_cleanup(
        open(pipe(Command), read, Fd),
        collect_line(Fd, String),
        close(Fd)),
    atom_codes(Pwd, String),
    same_file(Pwd, '.').
:- if(\+ current_prolog_flag(wine_version, _)).
test(cat1) :-
    current_prolog_flag(pid, Pid),
    format(atom(File), 'pltest-~w.txt', [Pid]),
    (   current_prolog_flag(windows, true)
    ->  format(atom(Bat), 'pltest-~w.bat', [Pid]),
        setup_call_cleanup(
            open(Bat, write, Fd1),
            writeln(Fd1, '@findstr .* > %1'),
            close(Fd1)),
        format(atom(Cmd), 'cmd /c ~w ~w', [Bat, File]),
        debug(pipe, 'Created BAT script ~q', [Bat])
    ;   format(atom(Cmd), 'cat > ~w', [File])
    ),
    Text = 'Hello World',
    setup_call_cleanup(
        open(pipe(Cmd), write, Fd2),
        format(Fd2, '~w~n', [Text]),
        close(Fd2)),
    debug(pipe, 'Wrote to ~p', [pipe(Cmd)]),
    setup_call_cleanup(
        open(File, read, Fd3),
        collect_data(Fd3, String),
        close(Fd3)),
    debug(pipe, 'Read ~q to "~s"', [File, String]),
    delete_file(File),
    (   nonvar(Bat)
    ->  delete_file(Bat)
    ;   true
    ),
    !,
    atom_codes(A, String),
    format(atom(A), '~w~n', [Text]).
:- endif.
test(cat2, [error(io_error(write, _)),
	    condition(not(wine))
	   ]) :-
    (   current_prolog_flag(windows, true)
    ->  Cmd = 'cmd /c rem true',
        Cleanup = true
    ;   Cmd = true,
        (   current_prolog_flag(signals, false)
        ->  on_signal(pipe, OldSigPipe, ignore),
            Cleanup = on_signal(pipe, _, OldSigPipe)
        ;   Cleanup = true
        )
    ),
    call_cleanup(
        setup_call_cleanup(
            open(pipe(Cmd), write, Pipe),
            forall(between(1, 120_000, _), format(Pipe, '0123456789~n', [])),
            close(Pipe, [force(true)])),
        Cleanup).

collect_line(Fd, String) :-
    get_code(Fd, C0),
    collect_line(C0, Fd, String).

collect_line(-1, _, []) :- !.
collect_line(10, _, []) :- !.
collect_line(13, _, []) :- !.
collect_line(C, Fd, [C|T]) :-
    get_code(Fd, C2),
    collect_line(C2, Fd, T).

collect_data(Fd, String) :-
    get_code(Fd, C0),
    collect_data(C0, Fd, String).

collect_data(-1, _, []) :- !.
collect_data(C, Fd, [C|T]) :-
    get_code(Fd, C2),
    collect_data(C2, Fd, T).


		 /*******************************
		 *	      TIMEOUT		*
		 *******************************/

test(timeout,
     [ condition(( current_prolog_flag(pipe, true),
                   \+ current_prolog_flag(windows, true)))
     ]) :-
    open(pipe('echo + && sleep 1 && echo xx.'), read, In,
         [ bom(false)
         ]),
    set_stream(In, timeout(0.2)),
    wait_for_input([In], [In], infinite),
    catch(read(In, Term1), E1, true),
    (	nonvar(E1),
        E1 = error(timeout_error(read, _), _)
    ->	wait_for_input([In], [In], infinite),
        catch(read(In, Term), E2, true),
        (   var(E2)
        ->  (   Term == xx
            ->	close(In)
            ;	format(user_error, 'Term == ~q~n', [Term]),
                true
            )
        ;   format(user_error, 'E2 == ~q~n', [E2]),
            fail
        )
    ;   var(E1)
    ->	(   Term1 == (+ xx)
        ->  close(In),
            format(user_error,
                   'timeout(pipe-1): ~q (machine heavy loaded?)~n',
                   [Term1])
        ;   format(user_error,
                   'var(E1) && Term == ~q~n', [Term1]),
            fail
        )
    ;	format(user_error, 'E1 == ~q~n', [E1]),
        fail
    ).

:- end_tests(pipe).
