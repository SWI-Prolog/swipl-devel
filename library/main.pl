/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2014, University of Amsterdam
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
	  [ main/0
	  ]).
:- use_module(library(lists)).

/** <module> Provide entry point for scripts

This library is intended for supporting   PrologScript on Unix using the
=|#!|= magic sequence for scripts using   commandline options. The entry
point main/0 calls the user-supplied predicate  main/1 passing a list of
commandline options. Below is `echo' in Prolog (adjust /usr/bin/swipl to
where SWI-Prolog is installed)

==
#!/usr/bin/swipl -q -g main -s

main(Argv) :-
	echo(Argv).

echo([]) :- nl.
echo([Last]) :- !,
	write(Last), nl.
echo([H|T]) :-
	write(H), write(' '),
	echo(T).
==

@see	XPCE users should have a look at library(pce_main), which
	starts the GUI and processes events until all windows have gone.
*/

:- module_transparent
	main/0.

%%	main
%
%	Call main/1 using the passed command-line arguments.

main :-
	context_module(M),
	set_signals,
	current_prolog_flag(argv, Av),
	run_main(M, Av).

%%	run_main(+Module, +Args)
%
%	Run the main routine, guarding for exceptions and failure of the
%	main/1 routine

run_main(Module, Av) :-
	(   catch(call(Module:main, Av), E, true)
	->  (   var(E)
	    ->	halt(0)
	    ;	print_message(error, E),
		halt(1)
	    )
	;   print_message(error, goal_failed(main(Av))),
	    halt(1)
	).

set_signals :-
	on_signal(int, _, interrupt).

%%	interrupt(+Signal)
%
%	We received an interrupt.  This handler is installed using
%	on_signal/3.

interrupt(_Sig) :-
	halt(1).

:- multifile
	prolog:called_by/2.

prolog:called_by(main, [main(_)]).
