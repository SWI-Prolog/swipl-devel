/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(subcommand,
	  [ pipe_through_command/3	% +Command, +In, -Out
	  ]).
:- use_module(library(unix)).

%	pipe_through_command(+Command, +In, -Out)
%
%	Pipe text In through command Command and put the output of command
%	in Out.
%	
%	Example:
%
%		?- pipe_through_command(tr('a-z', 'A-Z'), hello, X)
%
%		X = 'HELLO'

pipe_through_command(Command, In, Out) :-
	pipe(ChildIn, MeOut),
	pipe(MeIn,  ChildOut),
	fork(Pid),
	(   Pid == child,
	    close(MeOut),
	    close(MeIn),
	    dup(ChildIn, 0),
	    dup(ChildOut, 1),
	    close(ChildIn),
	    close(ChildOut),
	    exec(Command)
	;   close(ChildIn),
	    close(ChildOut),
	    write(MeOut, In),
	    close(MeOut),
	    read_input(MeIn, Out),
	    close(MeIn),
	    wait(Pid, _Status)		% avoid zombi
	).


read_input(In, Atom) :-
	get_code(In, C0),
	read_input(C0, In, Codes),
	atom_codes(Atom, Codes).

read_input(-1, _, []) :- !.
read_input(C, In, [C|T]) :-
	get_code(In, C1),
	read_input(C1, In, T).
	
