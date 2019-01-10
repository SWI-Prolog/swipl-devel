/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2017, University of Amsterdam
                              VU University Amsterdam
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

:- module(prolog_main,
          [ main/0,
            argv_options/3                      % +Argv, -RestArgv, -Options
          ]).

/** <module> Provide entry point for scripts

This library is intended for supporting   PrologScript on Unix using the
=|#!|= magic sequence for scripts using   commandline options. The entry
point main/0 calls the user-supplied predicate  main/1 passing a list of
commandline options. Below is a simle `echo` implementation in Prolog.

```
#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :-
    echo(Argv).

echo([]) :- nl.
echo([Last]) :- !,
    write(Last), nl.
echo([H|T]) :-
    write(H), write(' '),
    echo(T).
```

@see	library(optparse) for comprehensive option parsing.
@see	library(prolog_stack) to force backtraces in case of an
	uncaught exception.
@see    XPCE users should have a look at library(pce_main), which
        starts the GUI and processes events until all windows have gone.
*/

:- module_transparent
    main/0.

%!  main
%
%   Call main/1 using the passed  command-line arguments. Before calling
%   main/1  this  predicate  installs  a  signal  handler  for  =SIGINT=
%   (Control-C) that terminates the process with status 1.

main :-
    context_module(M),
    set_signals,
    current_prolog_flag(argv, Av),
    catch_with_backtrace(M:main(Av), Error, throw(Error)).

set_signals :-
    on_signal(int, _, interrupt).

%!  interrupt(+Signal)
%
%   We received an interrupt.  This handler is installed using
%   on_signal/3.

interrupt(_Sig) :-
    halt(1).

%!  argv_options(+Argv, -RestArgv, -Options) is det.
%
%   Generic transformation of long commandline arguments to options.
%   Each --Name=Value is mapped to Name(Value).   Each plain name is
%   mapped to Name(true), unless Name starts  with =|no-|=, in which
%   case the option is mapped to  Name(false). Numeric option values
%   are mapped to Prolog numbers.
%
%   @see library(optparse) provides a more involved option library,
%   providing both short and long options, help and error handling.
%   This predicate is more for quick-and-dirty scripts.

argv_options([], [], []).
argv_options([H0|T0], R, [H|T]) :-
    sub_atom(H0, 0, _, _, --),
    !,
    (   sub_atom(H0, B, _, A, =)
    ->  B2 is B-2,
        sub_atom(H0, 2, B2, _, Name),
        sub_string(H0, _, A,  0, Value0),
        convert_option(Name, Value0, Value)
    ;   sub_atom(H0, 2, _, 0, Name0),
        (   sub_atom(Name0, 0, _, _, 'no-')
        ->  sub_atom(Name0, 3, _, 0, Name),
            Value = false
        ;   Name = Name0,
            Value = true
        )
    ),
    H =.. [Name,Value],
    argv_options(T0, R, T).
argv_options([H|T0], [H|R], T) :-
    argv_options(T0, R, T).

convert_option(password, String, String) :- !.
convert_option(_, String, Number) :-
    number_string(Number, String),
    !.
convert_option(_, String, Atom) :-
    atom_string(Atom, String).

:- multifile
    prolog:called_by/2.

prolog:called_by(main, [main(_)]).
