/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(sics_system,
	  [ environ/2,			% ?Name, ?Value
	    exec/3,
	    working_directory/2,
	    wait/2
	  ]).
:- use_module(library(process)).

/** <module> SICStus-3 library system


@tbd	This library is incomplete
*/

%%	environ(?Name, ?Value) is nondet.
%
%	True if Value an atom associated   with the environment variable
%	Name.
%
%	@tbd	Mode -Name is not supported

environ(Name, Value) :-
	getenv(Name, Value).

%%	exec(+Command, +Streams, -PID)
%
%	SICStus 3 compatible implementation of  exec/3   on  top  of the
%	SICStus 4 compatible process_create/3.

exec(Command, Streams, PID) :-
	Streams = [In, Out, Error],
	shell(Shell, Command, Argv),
	process_create(Shell, Argv,
		       [ stdin(In),
			 stdout(Out),
			 stderr(Error),
			 process(PID)
		       ]).

shell('cmd.exe', Command, ['/C', Command]) :-
	current_prolog_flag(windows, true), !.
shell('/bin/sh', Command, ['-c', Command]).

%%	wait(+PID, -Status)
%
%	Wait for processes created using exec/3.
%
%	@see exec/3

wait(PID, Status) :-
	process_wait(PID, Status).
